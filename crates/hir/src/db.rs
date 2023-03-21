use base_db::{salsa, FileId, FilePathId};

#[salsa::query_group(HirDatabaseStorage)]
pub trait HirDatabase: base_db::SourceDatabase {
    #[salsa::invoke(crate::file_hir::file_sema_query)]
    fn file_sema(&self, file_id: FilePathId) -> crate::FileSema;
}
