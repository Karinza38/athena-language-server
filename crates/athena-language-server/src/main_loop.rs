use dashmap::DashMap;
use ide::{Analysis, AnalysisHost, Cancellable};
use ide_db::base_db::Change;
use parking_lot::RwLock;
use std::{path::PathBuf, sync::Arc};
use tokio::sync::{
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    oneshot,
};
use tower_lsp::{
    async_trait,
    jsonrpc::{Error as LspError, Result as LspResult},
    lsp_types::{
        Diagnostic, DidChangeConfigurationParams, DidChangeTextDocumentParams,
        DidChangeWatchedFilesParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFilter, InitializeParams,
        InitializeResult, MessageType, OneOf, SemanticTokens, SemanticTokensFullOptions,
        SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
        SemanticTokensRegistrationOptions, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, StaticRegistrationOptions, TextDocumentRegistrationOptions,
        TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
        WorkspaceFoldersServerCapabilities, WorkspaceServerCapabilities,
    },
    Client, LanguageServer, LspService, Server,
};
use vfs::{FileId, Vfs};

use crate::{from_proto, semantic_tokens, to_proto};

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

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
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
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
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
        let (send, recv) = oneshot::channel();
        let handler = move |snapshot: GlobalStateSnapshot| {
            let out = crate::handlers::semantic_tokens_full(snapshot, params);
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
        // Ok(None)
    }

    async fn shutdown(&self) -> LspResult<()> {
        self.state_client.shutdown();
        Ok(())
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
}

#[derive(Debug)]
struct StateClient {
    sender: UnboundedSender<StateClientMessage>,
}

impl StateClient {
    fn new(sender: UnboundedSender<StateClientMessage>) -> Self {
        Self { sender }
    }

    fn handle_request(&self, handler: Handler) {
        self.sender
            .send(StateClientMessage::HandleRequest(handler))
            .unwrap();
    }

    fn shutdown(&self) {
        self.sender.send(StateClientMessage::Shutdown).unwrap();
    }

    fn files_changed(&self) {
        self.sender.send(StateClientMessage::FilesChanged).unwrap();
    }
}

pub(crate) struct GlobalState {
    analysis_host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    client: Option<Client>,
    receiver: UnboundedReceiver<StateClientMessage>,
}

pub(crate) struct GlobalStateSnapshot {
    pub(crate) analysis: Analysis,
    vfs: Arc<RwLock<Vfs>>,
    pub(crate) semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    #[allow(dead_code)]
    pub(crate) client: Client,
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

type Handler = Box<dyn FnOnce(GlobalStateSnapshot) + Send>;

enum StateClientMessage {
    HandleRequest(Handler),

    FilesChanged,

    Shutdown,
}

impl std::fmt::Debug for StateClientMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HandleRequest(_) => write!(f, "HandleRequest"),
            Self::FilesChanged => write!(f, "FilesChanged"),
            Self::Shutdown => write!(f, "Shutdown"),
        }
    }
}

impl GlobalStateSnapshot {
    pub(crate) fn file_id(&self, url: &Url) -> crate::Result<FileId> {
        from_proto::url_to_file_id(&*self.vfs.read(), url)
    }
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
        .append(true)
        .create(true)
        .open(pth.clone())
        .unwrap();

    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .with_ansi(false)
        .with_writer(logfile)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (state_client, mut state) = GlobalState::new();

    let vfs = state.vfs.clone();
    let (service, socket) = LspService::new(move |c| Backend::new(c, vfs, state_client));

    state.client = Some(service.inner().client.clone());

    tokio::spawn(state.run());

    Server::new(stdin, stdout, socket).serve(service).await;
}
