mod line_index;
mod semantic_tokens;

use dashmap::DashMap;
use ropey::Rope;
use syntax::SourceFile;
use tower_lsp::{
    async_trait,
    jsonrpc::Result,
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

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    ast_map: DashMap<String, syntax::Parse<SourceFile>>,
    semantic_token_map: DashMap<String, SemanticTokens>,
}

#[derive(Debug)]
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            document_map: DashMap::new(),
            ast_map: DashMap::new(),
            semantic_token_map: DashMap::new(),
        }
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = self.semantic_token_map.get(&uri).map(|v| v.clone());
        if let Some(semantic_tokens) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(semantic_tokens)));
        } else {
            let Some(ast) = self.ast_map.get(&uri).map(|v| v.clone()) else {
                return Ok(None);
            };
            let Some(rop) = self.document_map.get(&uri).map(|v| v.clone()) else {
                return Ok(None);
            };
            let index = line_index::LineIndex::new(&rop.to_string());
            let tokens = semantic_tokens::semantic_tokens_for_file(ast.tree(), &index);
            self.semantic_token_map.insert(uri, tokens.clone());
            return Ok(Some(SemanticTokensResult::Tokens(tokens)));
        }
        // Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        self.client.log_message(MessageType::LOG, "on_change").await;
        let rope = Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        let ast = syntax::SourceFile::parse(&params.text);
        let index = line_index::LineIndex::new(&params.text);
        let diagnostics = ast
            .errors()
            .iter()
            .map(|err| {
                let range = index.range(err.range());
                Diagnostic::new_simple(range, err.message().to_owned())
            })
            .collect::<Vec<_>>();

        let tokens = semantic_tokens::semantic_tokens_for_file(ast.tree(), &index);

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
        self.ast_map.insert(params.uri.to_string(), ast);
        self.semantic_token_map
            .insert(params.uri.to_string(), tokens);
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
