mod from_proto;
mod semantic_tokens;
mod to_proto;

use dashmap::DashMap;
use ide::{Analysis, AnalysisHost, Cancellable};
use ide_db::base_db::Change;
use parking_lot::RwLock;
use std::{future::Future, sync::Arc};
use tokio::{
    sync::{mpsc::Sender, oneshot},
    task::JoinHandle,
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

pub type Error = Box<dyn std::error::Error + Send + Sync>;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
struct Backend {
    client: Client,
    analysis_client: AnalysisClient,
    vfs: RwLock<Vfs>,
    semantic_token_map: DashMap<String, SemanticTokens>,
}

#[derive(Debug)]
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    fn new(client: Client, analysis_client: AnalysisClient) -> Self {
        Self {
            client,
            semantic_token_map: DashMap::new(),
            analysis_client,
            vfs: RwLock::new(Vfs::default()),
        }
    }

    async fn analysis(&self) -> Analysis {
        self.analysis_client.analysis().await.await.unwrap()
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
        let analysis = self.analysis().await;
        let file_id = self.file_id(&params.text_document.uri)?;
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = self.semantic_token_map.get(&uri).map(|v| v.clone());
        if let Some(semantic_tokens) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(semantic_tokens)));
        } else {
            let index = analysis.file_line_index(file_id).cancellable()?;
            let ast = analysis.parse(file_id).cancellable()?;

            let tokens = semantic_tokens::semantic_tokens_for_file(ast.tree(), &index);
            self.semantic_token_map.insert(uri, tokens.clone());
            return Ok(Some(SemanticTokensResult::Tokens(tokens)));
        }
        // Ok(None)
    }

    async fn shutdown(&self) -> LspResult<()> {
        self.analysis_client.shutdown().await;
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
    fn file_id(&self, uri: &Url) -> LspResult<FileId> {
        let vfs = self.vfs.read();
        from_proto::url_to_file_id(&vfs, uri)
            .map_err(|err| LspError::invalid_params(format!("invalid file path: {}", err)))
    }
    async fn on_change(&self, params: TextDocumentItem) {
        self.client.log_message(MessageType::LOG, "on_change").await;
        let pth = from_proto::vfs_path(&params.uri).unwrap();
        let change = {
            let mut vfs = self.vfs.write();
            vfs.set_file_contents(pth, Some(params.text.into_bytes()));

            let changes = vfs.take_changes();

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
        self.analysis_client.files_changed(change).await;

        let analysis = self.analysis().await;
        let file_id = self.file_id(&params.uri).unwrap();

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

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
        self.semantic_token_map
            .insert(params.uri.to_string(), tokens);
    }
}

#[derive(Debug)]
enum AnalysisClientMessage {
    WantAnalysis(oneshot::Sender<Analysis>),

    FilesChanged(Change),

    Shutdown,
}

#[derive(Debug)]
struct AnalysisClient {
    sender: Sender<AnalysisClientMessage>,
}

impl AnalysisClient {
    async fn analysis(&self) -> impl Future<Output = Result<Analysis, oneshot::error::RecvError>> {
        let (sender, receiver) = oneshot::channel();
        self.sender
            .send(AnalysisClientMessage::WantAnalysis(sender))
            .await
            .unwrap();
        receiver
    }

    async fn files_changed(&self, change: Change) {
        self.sender
            .send(AnalysisClientMessage::FilesChanged(change))
            .await
            .unwrap();
    }

    async fn shutdown(&self) {
        self.sender
            .send(AnalysisClientMessage::Shutdown)
            .await
            .unwrap();
    }
}

struct AnalysisHostTask(JoinHandle<()>);

fn start_analysis() -> (AnalysisClient, AnalysisHostTask) {
    const BUFFER_SIZE: usize = 1024 * 100;
    let (client_sender, client_receiver) = tokio::sync::mpsc::channel(BUFFER_SIZE);

    let host = tokio::spawn(async move {
        let mut client_receiver = client_receiver;
        let mut host = AnalysisHost::default();

        loop {
            let Some(msg) = client_receiver.recv().await else {
                break;
            };

            match msg {
                AnalysisClientMessage::WantAnalysis(sender) => {
                    let analysis = host.analysis();
                    sender.send(analysis).unwrap();
                }
                AnalysisClientMessage::FilesChanged(change) => {
                    host.apply_change(change);
                }
                AnalysisClientMessage::Shutdown => {
                    break;
                }
            }
        }
    });

    let client = AnalysisClient {
        sender: client_sender,
    };

    (client, AnalysisHostTask(host))
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (analysis_client, _host) = start_analysis();

    let (service, socket) = LspService::new(move |c| Backend::new(c, analysis_client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
