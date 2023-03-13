pub mod line_index;

mod apply_change;

use core::fmt;
use std::sync::Arc;

pub use base_db;

use base_db::{salsa, FileId};
use line_index::LineIndex;

#[salsa::database(base_db::SourceDatabaseStorage, LineIndexDatabaseStorage)]
pub struct RootDatabase {
    storage: salsa::Storage<RootDatabase>,
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
        })
    }
}

impl RootDatabase {
    pub fn new() -> RootDatabase {
        RootDatabase {
            storage: salsa::Storage::default(),
        }
    }
}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::new()
    }
}

#[salsa::query_group(LineIndexDatabaseStorage)]
pub trait LineIndexDatabase: base_db::SourceDatabase {
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id);
    Arc::new(LineIndex::new(&text))
}
