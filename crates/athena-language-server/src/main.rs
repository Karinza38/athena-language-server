mod semantic_tokens;

use tower_lsp::{
    async_trait,
    jsonrpc::Result,
    lsp_types::{InitializeParams, InitializeResult, MessageType},
    Client, LanguageServer, LspService, Server,
};

#[derive(Debug)]
struct Backend {
    client: Client,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self { client }
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: tower_lsp::lsp_types::InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
