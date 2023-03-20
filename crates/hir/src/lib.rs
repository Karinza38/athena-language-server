pub mod ded;
pub mod expr;
pub mod file_hir;
pub mod identifier;
pub mod item_tree;
pub mod module;
pub mod name;
pub mod pat;
pub mod phrase;
pub mod scope;
pub mod sort;

pub mod ast_mapping;
pub mod file_graph;

pub mod db;

pub use db::{HirDatabase, HirDatabaseStorage};

#[cfg(test)]
mod test_db;

pub use file_hir::{FileHir, FileHirSourceMap, FileSema, HasHir, HasSyntaxNodePtr, HirNode};
pub use scope::ScopeTree;

use base_db::FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> Self {
        Self { file_id, value }
    }
}
