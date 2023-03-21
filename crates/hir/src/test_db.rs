use core::fmt;

use crate::db::HirDatabase;
use base_db::{salsa, FileWatcher, Upcast};

#[derive(Default)]
#[salsa::database(base_db::SourceDatabaseStorage, crate::db::HirDatabaseStorage)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,
}

impl salsa::Database for TestDB {}

impl FileWatcher for TestDB {
    fn did_change_file(&mut self, _file_id: base_db::FilePathId) {}

    fn in_mem_contents(&self, _path: &base_db::AbsPath) -> Option<std::sync::Arc<String>> {
        None
    }

    fn set_in_mem_contents(
        &mut self,
        _path: base_db::AbsPathBuf,
        _contents: std::sync::Arc<String>,
    ) {
    }
}

impl Upcast<dyn HirDatabase> for TestDB {
    fn upcast(&self) -> &(dyn HirDatabase + 'static) {
        &*self
    }
}

impl fmt::Debug for TestDB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestDB").finish()
    }
}
