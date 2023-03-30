use std::sync::Arc;

use dashmap::DashMap;
use ide::{Analysis, AnalysisHost, Cancellable, LineIndex};
use ide_db::base_db::FileId;
use parking_lot::RwLock;
use paths::AbsPathBuf;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tower_lsp::{
    lsp_types::{SemanticTokens, Url},
    Client,
};

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

    pub(crate) fn files_changed(&self, files: impl IntoIterator<Item = (AbsPathBuf, String)>) {
        self.sender
            .send(StateClientMessage::FilesChanged(
                files.into_iter().collect(),
            ))
            .unwrap();
    }
}

pub(crate) struct GlobalState {
    pub(crate) analysis_host: AnalysisHost,
    pub(crate) semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    pub(crate) client: Option<Client>,
    pub(crate) receiver: UnboundedReceiver<StateClientMessage>,
    pub(crate) config: Arc<RwLock<crate::config::Config>>,
}

pub(crate) struct GlobalStateSnapshot {
    pub(crate) analysis: Analysis,
    pub(crate) semantic_token_map: Arc<DashMap<String, SemanticTokens>>,
    #[allow(dead_code)]
    pub(crate) client: Client,
    pub(crate) config: Arc<crate::config::Config>,
}
type Handler = Box<dyn FnOnce(GlobalStateSnapshot) + Send>;

pub(crate) enum StateClientMessage {
    HandleRequest(Handler),

    FilesChanged(Vec<(AbsPathBuf, String)>),

    Shutdown,
}

impl std::fmt::Debug for StateClientMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HandleRequest(_) => write!(f, "HandleRequest"),
            Self::FilesChanged(files) => f
                .debug_tuple("FilesChanged")
                .field(&files.iter().map(|(p, _)| p).collect::<Vec<_>>())
                .finish(),
            Self::Shutdown => write!(f, "Shutdown"),
        }
    }
}

impl GlobalStateSnapshot {
    pub(crate) fn file_id(&self, url: &Url) -> Cancellable<crate::Result<FileId>> {
        match from_proto::abs_path(url) {
            Ok(pth) => Ok(Ok(self.analysis.intern_path(pth)?)),
            Err(e) => Ok(Err(e)),
        }
    }

    pub(crate) fn file_id_to_url(&self, id: FileId) -> Cancellable<Url> {
        let path = self.analysis.file_path(id)?.unwrap();
        Ok(to_proto::url_from_abs_path(&path))
    }

    pub(crate) fn file_line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.analysis.file_line_index(file_id)
    }
}
