use dashmap::DashMap;
use ide::{AnalysisHost, Cancellable};
use ide_db::base_db::Change;
use parking_lot::RwLock;
use std::{fmt::Debug, path::PathBuf, sync::Arc};
use tokio::sync::{mpsc, oneshot};
use tower_lsp::{
    async_trait,
    jsonrpc::{Error as LspError, Result as LspResult},
    lsp_types::{
        request::{
            GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams,
            GotoImplementationResponse,
        },
        Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams,
        DidChangeWatchedFilesParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFilter, GotoDefinitionParams,
        GotoDefinitionResponse, InitializeParams, InitializeResult, MessageType, OneOf,
        SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensRegistrationOptions, SemanticTokensResult,
        SemanticTokensServerCapabilities, ServerCapabilities, StaticRegistrationOptions,
        TextDocumentIdentifier, TextDocumentRegistrationOptions, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url, WorkDoneProgressOptions, WorkspaceFoldersServerCapabilities,
        WorkspaceServerCapabilities,
    },
    Client, LanguageServer, LspService, Server,
};
use tracing_subscriber::EnvFilter;
use vfs::Vfs;

use crate::{
    from_proto,
    global_state::{GlobalState, GlobalStateSnapshot, StateClient, StateClientMessage},
    handlers, semantic_tokens, to_proto,
};

#[derive(Debug)]
struct Backend {
    client: Client,
    vfs: Arc<RwLock<Vfs>>,
    state_client: StateClient,
}

#[derive(Debug)]
struct TextDocumentItem {
    uri: Url,
    text: String,
    #[allow(dead_code)]
    version: i32,
}

impl Backend {
    fn new(client: Client, vfs: Arc<RwLock<Vfs>>, state_client: StateClient) -> Self {
        Self {
            client,
            vfs,
            state_client,
        }
    }
}

fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        semantic_tokens_provider: Some(
            SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                SemanticTokensRegistrationOptions {
                    text_document_registration_options: {
                        TextDocumentRegistrationOptions {
                            document_selector: Some(vec![DocumentFilter {
                                language: Some("athena".to_string()),
                                scheme: Some("file".to_string()),
                                pattern: None,
                            }]),
                        }
                    },
                    semantic_tokens_options: SemanticTokensOptions {
                        work_done_progress_options: WorkDoneProgressOptions::default(),
                        legend: SemanticTokensLegend {
                            token_types: semantic_tokens::SUPPORTED_TYPES.into(),
                            token_modifiers: vec![],
                        },
                        range: Some(false),
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                    },
                    static_registration_options: StaticRegistrationOptions::default(),
                },
            ),
        ),
        definition_provider: Some(OneOf::Left(true)),
        declaration_provider: Some(tower_lsp::lsp_types::DeclarationCapability::Simple(true)),
        implementation_provider: Some(
            tower_lsp::lsp_types::ImplementationProviderCapability::Simple(true),
        ),
        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: None,
        }),
        ..ServerCapabilities::default()
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: capabilities(),
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LspResult<Option<SemanticTokensResult>> {
        self.dispatch_request(params, handlers::semantic_tokens_full)
            .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LspResult<Option<GotoDefinitionResponse>> {
        self.dispatch_request(params, handlers::go_to_definition)
            .await
    }

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> LspResult<Option<GotoDeclarationResponse>> {
        self.dispatch_request(params, handlers::go_to_definition)
            .await
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> LspResult<Option<GotoImplementationResponse>> {
        self.dispatch_request(params, handlers::go_to_definition)
            .await
    }

    async fn shutdown(&self) -> LspResult<()> {
        self.state_client.shutdown();
        Ok(())
    }
}

impl Backend {
    async fn show_syntax_tree(&self, params: TextDocumentIdentifier) -> LspResult<String> {
        self.dispatch_request(params, handlers::dump_syntax_tree)
            .await
    }
}

#[extend::ext]
impl<T> Cancellable<T> {
    fn cancellable(self) -> Result<T, LspError> {
        self.map_err(|_| LspError::request_cancelled())
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        self.client.log_message(MessageType::LOG, "on_change").await;
        let pth = from_proto::vfs_path(&params.uri).unwrap();
        let _sp = tracing::info_span!("on_change");
        let mut vfs = self.vfs.write();
        vfs.set_file_contents(pth.clone(), Some(params.text.into_bytes()));

        tracing::info!("file contents set: {:?}", pth);

        self.state_client.files_changed();
    }

    async fn dispatch_request<P: Send + 'static, R: Send + Debug + 'static>(
        &self,
        params: P,
        handler: impl FnOnce(GlobalStateSnapshot, P) -> crate::Result<R> + Send + 'static,
    ) -> LspResult<R> {
        let (send, recv) = oneshot::channel();
        let handler = move |snapshot: GlobalStateSnapshot| {
            let out = handler(snapshot, params);
            send.send(out).unwrap();
        };
        self.state_client.handle_request(Box::new(handler));

        match recv.await.unwrap() {
            Ok(t) => Ok(t),
            Err(e) => {
                self.client
                    .log_message(MessageType::ERROR, format!("{e:#?}"))
                    .await;
                return Err(LspError::internal_error());
            }
        }
    }
}

impl GlobalState {
    fn new() -> (StateClient, GlobalState) {
        let vfs = Arc::new(RwLock::new(Vfs::default()));
        let analysis_host = AnalysisHost::new();
        let semantic_token_map = Arc::new(DashMap::new());
        let (sender, receiver) = mpsc::unbounded_channel();
        (
            StateClient::new(sender),
            GlobalState {
                analysis_host,
                vfs,
                semantic_token_map,
                client: None,
                receiver,
            },
        )
    }
    async fn run(self) {
        let Self {
            mut analysis_host,
            vfs,
            semantic_token_map,
            client,
            mut receiver,
        } = self;
        let client = client.unwrap();
        loop {
            match receiver.recv().await {
                Some(StateClientMessage::HandleRequest(handler)) => {
                    tracing::info!("handling request");
                    let analysis = analysis_host.analysis();
                    let vfs = vfs.clone();
                    let semantic_token_map = semantic_token_map.clone();
                    let client = client.clone();
                    tokio::task::spawn_blocking(move || {
                        let snapshot = GlobalStateSnapshot {
                            analysis,
                            vfs,
                            semantic_token_map,
                            client,
                        };
                        handler(snapshot);
                    });
                }
                Some(StateClientMessage::FilesChanged) => {
                    let diagnostics = on_changed(&vfs, &mut analysis_host, &semantic_token_map);
                    for (url, diagnostic) in diagnostics {
                        let client = client.clone();
                        tokio::spawn(async move {
                            client.publish_diagnostics(url, diagnostic, None).await;
                        });
                    }
                }
                Some(StateClientMessage::Shutdown) => {
                    break;
                }
                None => break,
            }
        }
    }
}

fn on_changed(
    vfs: &Arc<RwLock<Vfs>>,
    host: &mut AnalysisHost,
    token_map: &DashMap<String, SemanticTokens>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let change = {
        let mut vfs = vfs.write();
        let changes = vfs.take_changes();

        tracing::info!("changes: {:?}", changes);

        let mut change = Change::new();
        for ch in changes {
            if ch.exists() {
                let contents = String::from_utf8(vfs.file_contents(ch.file_id).to_vec())
                    .ok()
                    .map(|c| Arc::new(c));
                change.change_file(ch.file_id, contents);
            } else {
                change.change_file(ch.file_id, None);
            }
        }
        change
    };

    let changed_files = change
        .files_changed
        .iter()
        .map(|(file_id, _)| *file_id)
        .collect::<Vec<_>>();
    host.apply_change(change);

    tracing::info!("applied change: {:?}", changed_files);

    let analysis = host.analysis();

    let mut file_diagnostics = Vec::new();
    for file_id in changed_files {
        // vfs.read().file_path(file_id).as_path().unwrap().
        let url = Url::from_file_path(vfs.read().file_path(file_id).as_path().unwrap()).unwrap();
        let ast = analysis.parse(file_id).unwrap();
        let index = analysis.file_line_index(file_id).unwrap();

        let diagnostics = ast
            .errors()
            .iter()
            .map(|err| {
                let range = to_proto::range(&index, err.range());
                Diagnostic::new_simple(range, err.message().to_owned())
            })
            .collect::<Vec<_>>();

        let tokens = semantic_tokens::semantic_tokens_for_file(ast.tree(), &index);

        token_map.insert(url.to_string(), tokens);

        file_diagnostics.push((url, diagnostics));
    }
    file_diagnostics
}

pub async fn run_server() {
    let pth = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("logfile.txt");

    let logfile = std::fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(pth.clone())
        .unwrap();

    use tracing_subscriber::prelude::*;

    tracing_subscriber::registry()
        .with(
            tracing_tree::HierarchicalLayer::new(2)
                .with_ansi(true)
                .with_writer(logfile),
        )
        .with(EnvFilter::from_default_env())
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (state_client, mut state) = GlobalState::new();

    let vfs = state.vfs();
    let (service, socket) = LspService::build(move |c| Backend::new(c, vfs, state_client))
        .custom_method(
            "athena-language-server/dumpSyntaxTree",
            Backend::show_syntax_tree,
        )
        .finish();

    state.client = Some(service.inner().client.clone());

    tokio::spawn(state.run());

    Server::new(stdin, stdout, socket).serve(service).await;
}
