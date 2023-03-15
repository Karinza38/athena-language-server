use base_db::impl_intern_key;
use base_db::salsa::InternId;
use la_arena::{Arena, Idx};
use std::collections::HashMap;
use std::marker::PhantomData;
use syntax::{ast, AstNode};

use crate::ast_mapping::FileAstId;
use crate::expr::Expr;
use crate::name::Name;
use crate::phrase::{Phrase, PhraseId};

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
    // top_level: Vec
}

pub struct ModuleData {
    definitions: Arena<Definition>,
    data_types: Arena<DataType>,
    sub_modules: Arena<Module>,
}

pub enum ModuleItem {
    Module(ModuleId),
    Definition(DefinitionId),
    DataType(DataTypeId),
    Structure(StructureId),
    Phrase(PhraseId),
}

pub enum Visibility {
    Public,
    Private,
}

pub enum DefKind {
    FunctionSym,
    Value,
    Sort,
}

pub struct Definition {
    name: Name,
    parent: ModuleId,
    visibility: Visibility,
    kind: DefKind,
}

pub struct DataType {
    name: Name,
    parent: ModuleId,
}

pub struct DataTypeConstructor {
    parent: DataTypeId,
    name: Name,
}

pub struct Structure {
    name: Name,
    parent: ModuleId,
}

pub type DataTypeId = Idx<DataType>;

pub type DefinitionId = Idx<Definition>;

pub type StructureId = Idx<Structure>;

pub struct ModuleId(InternId);

impl_intern_key!(ModuleId);

pub enum ModuleKind {
    Definition,
    Extension,
}

pub struct Module {
    pub kind: ModuleKind,
    pub name: Name,
    pub ast_id: FileAstId<ast::Module>,
}

pub struct ModuleSourceMap {}
