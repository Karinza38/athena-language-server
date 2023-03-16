use core::fmt;

use crate::db::HirDatabase;
use base_db::{salsa, Upcast};

#[derive(Default)]
#[salsa::database(base_db::SourceDatabaseStorage, crate::db::HirDatabaseStorage)]
pub(crate) struct TestDB {
    storage: salsa::Storage<TestDB>,
}

impl salsa::Database for TestDB {}

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
