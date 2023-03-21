pub mod line_index;

mod apply_change;

use core::fmt;
use std::sync::Arc;

pub use base_db;

use base_db::{salsa, FileContentsQuery, FilePathId, FileWatcher};
use line_index::LineIndex;
use rustc_hash::FxHashMap;

#[salsa::database(
    base_db::SourceDatabaseStorage,
    LineIndexDatabase2Storage,
    hir::HirDatabaseStorage
)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
    in_mem_files: Arc<FxHashMap<base_db::AbsPathBuf, Arc<String>>>,
}

impl salsa::Database for RootDatabase {}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish()
    }
}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
            in_mem_files: self.in_mem_files.clone(),
        })
    }
}

impl RootDatabase {
    pub fn new() -> RootDatabase {
        RootDatabase {
            storage: salsa::Storage::default(),
            in_mem_files: Default::default(),
        }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::new()
    }
}

impl FileWatcher for RootDatabase {
    #[tracing::instrument(skip(self))]
    fn did_change_file(&mut self, file_id: FilePathId) {
        tracing::debug!("acquiring lock maybe?");
        FileContentsQuery.in_db_mut(self).invalidate(&file_id)
    }

    fn in_mem_contents(&self, path: &base_db::AbsPath) -> Option<Arc<String>> {
        self.in_mem_files.get(path).cloned()
    }

    fn set_in_mem_contents(&mut self, path: base_db::AbsPathBuf, contents: Arc<String>) {
        Arc::make_mut(&mut self.in_mem_files).insert(path, contents);
    }
}

#[salsa::query_group(LineIndexDatabase2Storage)]
pub trait LineIndexDatabase2: base_db::SourceDatabase {
    fn line_index2(&self, file_id: FilePathId) -> Arc<LineIndex>;
}

fn line_index2(db: &dyn LineIndexDatabase2, file_id: FilePathId) -> Arc<LineIndex> {
    let text = db.file_contents(file_id);
    Arc::new(LineIndex::new(&text))
}
