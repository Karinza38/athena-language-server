use std::sync::Arc;

use anyhow::anyhow;
use dashmap::DashMap;
use ide::{Analysis, AnalysisHost, Cancellable, FileId, LineIndex};
use parking_lot::RwLock;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tower_lsp::{
    lsp_types::{self, SemanticTokens, Url},
    Client,
};
use vfs::Vfs;

use crate::{from_proto, to_proto};

#[derive(Debug)]
pub(crate) struct StateClient {
    sender: UnboundedSender<StateClientMessage>,
}

impl StateClient {
    pub(crate) fn new(sender: UnboundedSender<StateClientMessage>) -> Self {
        Self { sender }
    }

    pub(crate) fn handle_request(&self, handler: Handler) {
        self.sender
            .send(StateClientMessage::HandleRequest(handler))
            .unwrap();
    }

    pub(crate) fn shutdown(&self) {
        self.sender.send(StateClientMessage::Shutdown).unwrap();
    }

    pub(crate) fn files_changed(&self) {
        self.sender.send(StateClientMessage::FilesChanged).unwrap();
    }
}

pub(crate) struct GlobalState {
    pub(crate) analysis_host: AnalysisHost,
    pub(crate) vfs: Arc<RwLock<Vfs>>,
    pub(crate) semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    pub(crate) client: Option<Client>,
    pub(crate) receiver: UnboundedReceiver<StateClientMessage>,
}

pub(crate) struct GlobalStateSnapshot {
    pub(crate) analysis: Analysis,
    pub(crate) vfs: Arc<RwLock<Vfs>>,
    pub(crate) semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    #[allow(dead_code)]
    pub(crate) client: Client,
}
type Handler = Box<dyn FnOnce(GlobalStateSnapshot) + Send>;

pub(crate) enum StateClientMessage {
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

impl GlobalState {
    pub(crate) fn vfs(&self) -> Arc<RwLock<Vfs>> {
        self.vfs.clone()
    }
}

impl GlobalStateSnapshot {
    pub(crate) fn file_id(&self, url: &Url) -> crate::Result<FileId> {
        url_to_file_id(&*self.vfs.read(), url)
    }

    pub(crate) fn file_id_to_url(&self, id: FileId) -> Url {
        file_id_to_url(&*self.vfs.read(), id)
    }

    pub(crate) fn file_line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.analysis.file_line_index(file_id)
    }
}

pub(crate) fn url_to_file_id(vfs: &vfs::Vfs, url: &lsp_types::Url) -> crate::Result<FileId> {
    let path = from_proto::vfs_path(url)?;
    tracing::info!(?path, "url_to_file_id");
    vfs.file_id(&path)
        .ok_or_else(|| anyhow!("file not found: {}", url).into())
}

pub(crate) fn file_id_to_url(vfs: &vfs::Vfs, id: FileId) -> Url {
    let path = vfs.file_path(id);
    let path = path.as_path().unwrap();
    to_proto::url_from_abs_path(path)
}
