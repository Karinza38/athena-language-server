use dashmap::DashMap;
use ide::{AnalysisHost, Cancellable, SourceDatabase};
use ide_db::base_db::FileWatcher;
use parking_lot::RwLock;
use paths::AbsPathBuf;
use std::{fmt::Debug, future::Future, path::PathBuf, sync::Arc};
use tokio::sync::{mpsc, oneshot};
use tower_lsp::{
    async_trait,
    jsonrpc::{Error as LspError, Result as LspResult},
    lsp_types::{
        request::{
            GotoDeclarationParams, GotoDeclarationResponse, GotoImplementationParams,
            GotoImplementationResponse,
        },
        ConfigurationItem, Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams,
        DidChangeWatchedFilesParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFilter, GotoDefinitionParams,
        GotoDefinitionResponse, InitializeParams, InitializeResult, MessageType, OneOf,
        Registration, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
        SemanticTokensOptions, SemanticTokensParams, SemanticTokensRegistrationOptions,
        SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities,
        StaticRegistrationOptions, TextDocumentIdentifier, TextDocumentRegistrationOptions,
        TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
        WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
    },
    Client, LanguageServer, LspService, Server,
};
use tracing_subscriber::EnvFilter;

use crate::{
    config, from_proto,
    global_state::{GlobalState, GlobalStateSnapshot, StateClient, StateClientMessage},
    handlers, semantic_tokens, to_proto,
};

#[derive(Debug)]
struct Backend {
    client: Client,
    state_client: StateClient,
    config: RwLock<config::Config>,
}

#[derive(Debug)]
struct TextDocumentItem {
    uri: Url,
    text: String,
    #[allow(dead_code)]
    version: i32,
}

impl Backend {
    fn new(client: Client, state_client: StateClient) -> Self {
        Self {
            client,
            state_client,
            config: RwLock::new(config::Config::default()),
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

#[extend::ext]
impl<T, E> Result<T, E>
where
    E: std::error::Error,
{
    fn unwrap_or_log(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => {
                tracing::error!("error: {}", e);
                panic!("error: {}", e);
            }
        }
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> LspResult<InitializeResult> {
        tracing::info!("initialize: {:?}", params);
        Ok(InitializeResult {
            server_info: None,
            capabilities: capabilities(),
        })
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
        self.client
            .register_capability(vec![Registration {
                id: "config-change-watch".to_string(),
                method: "workspace/didChangeConfiguration".to_string(),
                register_options: None,
            }])
            .await
            .unwrap_or_log();
        self.refresh_config().await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
        tracing::info!("config changed");
        self.refresh_config().await;
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

    #[tracing::instrument(skip(self, params))]
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
        self.config_gated(
            |c| c.wip_goto_definition_enable,
            || async move {
                self.dispatch_request(params, handlers::go_to_definition)
                    .await
            },
        )
        .await
    }

    async fn goto_declaration(
        &self,
        params: GotoDeclarationParams,
    ) -> LspResult<Option<GotoDeclarationResponse>> {
        self.config_gated(
            |c| c.wip_goto_definition_enable,
            || async {
                self.dispatch_request(params, handlers::go_to_definition)
                    .await
            },
        )
        .await
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> LspResult<Option<GotoImplementationResponse>> {
        self.config_gated(
            |c| c.wip_goto_definition_enable,
            || async {
                self.dispatch_request(params, handlers::go_to_definition)
                    .await
            },
        )
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
        let abs_pth = from_proto::abs_path(&params.uri).unwrap();
        let _sp = tracing::info_span!("on_change");

        self.state_client
            .files_changed(vec![(abs_pth, params.text)]);
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
                Err(LspError::internal_error())
            }
        }
    }

    async fn refresh_config(&self) {
        if let Ok(config) = self
            .client
            .configuration(vec![ConfigurationItem {
                section: Some("athena-language-server".to_string()),
                ..Default::default()
            }])
            .await
        {
            if config.len() == 1 {
                let config: config::Config =
                    serde_json::from_value(config.into_iter().next().unwrap()).unwrap_or_log();
                tracing::info!("config: {:?}", config);
                *self.config.write() = config;
            } else {
                tracing::warn!("failed to deserialize config: {config:?}");
            }
        } else {
            tracing::warn!("failed to get config");
        }
    }

    async fn config_gated<F, Fut, R>(
        &self,
        enabled: impl FnOnce(&config::Config) -> bool,
        func: F,
    ) -> LspResult<Option<R>>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = LspResult<Option<R>>>,
    {
        let enabled = {
            let config = self.config.read();
            enabled(&config)
        };
        if enabled {
            func().await
        } else {
            Ok(None)
        }
    }
}

impl GlobalState {
    fn new() -> (StateClient, GlobalState) {
        let analysis_host = AnalysisHost::new();
        let semantic_token_map = Arc::new(DashMap::new());
        let (sender, receiver) = mpsc::unbounded_channel();
        (
            StateClient::new(sender),
            GlobalState {
                analysis_host,
                semantic_token_map,
                client: None,
                receiver,
            },
        )
    }
    async fn run(self) {
        let Self {
            mut analysis_host,
            semantic_token_map,
            client,
            mut receiver,
        } = self;
        let client = client.unwrap();
        loop {
            match receiver.recv().await {
                Some(StateClientMessage::HandleRequest(handler)) => {
                    let _sp = tracing::info_span!("handling request");
                    let analysis = analysis_host.analysis();
                    let semantic_token_map = semantic_token_map.clone();
                    let client = client.clone();
                    tokio::task::spawn_blocking(move || {
                        let snapshot = GlobalStateSnapshot {
                            analysis,
                            semantic_token_map,
                            client,
                        };
                        handler(snapshot);
                    });
                }
                Some(StateClientMessage::FilesChanged(files)) => {
                    let _sp = tracing::info_span!("files changed");
                    tracing::info!(
                        "files changed: {:?}",
                        files.iter().map(|(p, _)| p).collect::<Vec<_>>()
                    );
                    let diagnostics = on_changed(&mut analysis_host, &semantic_token_map, files);
                    tracing::debug!("now here");
                    for (url, diagnostic) in diagnostics {
                        let client = client.clone();
                        tokio::spawn(async move {
                            client.publish_diagnostics(url, diagnostic, None).await;
                        });
                    }
                    tracing::info!("files changed: done");
                }
                Some(StateClientMessage::Shutdown) => {
                    break;
                }
                None => break,
            }
        }
    }
}

#[tracing::instrument(skip(host, token_map, files))]
fn on_changed(
    host: &mut AnalysisHost,
    token_map: &DashMap<String, SemanticTokens>,
    files: Vec<(AbsPathBuf, String)>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    tracing::info!(
        "applied change: {:?}",
        files.iter().map(|(p, _)| p).collect::<Vec<_>>()
    );

    let mut file_diagnostics = Vec::new();
    for (file_path, file_contents) in files {
        let contents = Arc::new(file_contents);
        host.raw_db_mut()
            .set_in_mem_contents(file_path.clone(), contents.clone());
        let file_id = host.raw_db().intern_path(file_path.clone().into());
        host.did_change_file(file_id);
        tracing::debug!("did change it");
        let analysis = host.analysis();
        let url = Url::from_file_path(file_path).unwrap();
        let ast = analysis.parse(file_id).unwrap();
        tracing::debug!("parsed it");
        let index = analysis.file_line_index(file_id).unwrap();

        let diagnostics = ast
            .errors()
            .iter()
            .map(|err| {
                let range = to_proto::range(&index, err.range());
                Diagnostic::new_simple(range, err.message().to_owned())
            })
            .collect::<Vec<_>>();

        let highlights = analysis.highlight(file_id).unwrap();
        let tokens = to_proto::semantic_tokens(&contents, &index, highlights);

        token_map.insert(url.to_string(), tokens);

        file_diagnostics.push((url, diagnostics));
    }
    file_diagnostics
}

fn init_logging() {
    use tracing_subscriber::prelude::*;

    if let Ok(log_file_pth) = std::env::var("RUST_LOG_FILE") {
        let pth = if log_file_pth == "1" || log_file_pth == "true" {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("logfile.txt")
        } else {
            PathBuf::from(log_file_pth)
        };
        let logfile = std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(pth)
            .unwrap();

        tracing_subscriber::registry()
            .with(EnvFilter::from_default_env())
            .with(
                tracing_tree::HierarchicalLayer::new(2)
                    .with_ansi(true)
                    .with_writer(logfile),
            )
            .init();
    } else {
        tracing_subscriber::registry()
            .with(tracing_tree::HierarchicalLayer::new(2).with_ansi(true))
            .with(EnvFilter::from_default_env())
            .init();
    }
}

pub async fn run_server() {
    init_logging();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (state_client, mut state) = GlobalState::new();

    let (service, socket) = LspService::build(move |c| Backend::new(c, state_client))
        .custom_method(
            "athena-language-server/dumpSyntaxTree",
            Backend::show_syntax_tree,
        )
        .finish();

    state.client = Some(service.inner().client.clone());

    tokio::spawn(state.run());

    Server::new(stdin, stdout, socket).serve(service).await;
}
