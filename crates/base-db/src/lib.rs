mod change;
mod input;

use std::sync::Arc;

pub use change::Change;
pub use input::{SourceRoot, SourceRootId};
pub use salsa::{self, Cancelled};
use syntax::{ast, Parse, TextRange, TextSize};
pub use vfs::{file_set::FileSet, AnchoredPath, AnchoredPathBuf, FileId, VfsPath};

#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl $crate::salsa::InternKey for $name {
            fn from_intern_id(v: $crate::salsa::InternId) -> Self {
                $name(v)
            }
            fn as_intern_id(&self) -> $crate::salsa::InternId {
                self.0
            }
        }
    };
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

#[derive(Clone, Copy, Debug)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

pub const DEFAULT_LRU_CAP: usize = 128;

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase: std::fmt::Debug {
    #[salsa::invoke(parse_query)]
    fn parse(&self, file_id: FileId) -> Parse<ast::SourceFile>;

    #[salsa::input]
    fn file_text(&self, file_id: FileId) -> Arc<String>;

    #[salsa::input]
    fn file_source_root(&self, file_id: FileId) -> SourceRootId;

    #[salsa::input]
    fn source_root(&self, source_root_id: SourceRootId) -> Arc<SourceRoot>;
}

impl dyn SourceDatabase {
    pub fn resolve_path(&self, path: AnchoredPath<'_>) -> Option<FileId> {
        let source_root_id = self.file_source_root(path.anchor);
        let source_root = self.source_root(source_root_id);
        source_root.resolve_path(path)
    }
}

fn parse_query(db: &dyn SourceDatabase, file_id: FileId) -> Parse<ast::SourceFile> {
    let text = db.file_text(file_id);
    ast::SourceFile::parse(&text)
}
