use base_db::impl_intern_key;
use base_db::salsa::InternId;
use la_arena::{Arena, Idx};
use std::collections::HashMap;
use std::marker::PhantomData;
use syntax::{ast, AstNode};

use crate::ast_mapping::FileAstId;
use crate::expr::Expr;
use crate::file_hir::ModuleItem;
use crate::name::Name;
use crate::phrase::PhraseId;

pub trait ItemTreeNode: Clone {
    type Source: AstNode;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self;

    fn id_from_module_item(mod_item: ModuleItem) -> Option<FileItemTreeId<Self>>;
}

pub struct FileItemTreeId<N: ItemTreeNode> {
    index: Idx<N>,
    _p: PhantomData<N>,
}

pub struct ItemTree {
    top_level: Vec<ModuleItem>,
}

pub struct ModuleSourceMap {}
