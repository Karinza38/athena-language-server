pub mod expr;
pub mod module;
pub mod name;

pub mod ast_mapping;
pub mod file_graph;

use base_db::FileId;

pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}
