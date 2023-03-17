mod lower;

use std::sync::Arc;

use base_db::FileId;
use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;
use syntax::{ast, AstPtr};

use crate::{
    db::HirDatabase,
    ded::{Ded, DedId},
    expr::{Expr, ExprId},
    name::Name,
    phrase::PhraseId,
    scope::{ScopeId, ScopeTree},
    sort::{Sort, SortId},
    InFile,
};

#[derive(Default, PartialEq, Eq, Debug)]
pub struct FileHir {
    pub exprs: Arena<Expr>,
    pub deds: Arena<Ded>,
    pub modules: Arena<Module>,
    pub definitions: Arena<Definition>,
    pub data_types: Arena<DataType>,
    pub structures: Arena<Structure>,
    pub sorts: Arena<Sort>,
}

pub enum DefId {
    Module(ModuleId),
    Definition(DefinitionId),
    DataType(DataTypeId),
    Structure(StructureId),
}

#[derive(Default, PartialEq, Eq, Debug)]
pub struct FileHirSourceMap {
    pub exprs: FxHashMap<ExprSource, ExprId>,
    pub exprs_back: ArenaMap<ExprId, ExprSource>,

    pub deds: FxHashMap<DedSource, DedId>,
    pub deds_back: ArenaMap<DedId, DedSource>,

    pub modules: FxHashMap<ModuleSource, ModuleId>,
    pub modules_back: ArenaMap<ModuleId, ModuleSource>,

    pub definitions: FxHashMap<DefinitionSource, DefinitionId>,
    pub definitions_back: ArenaMap<DefinitionId, DefinitionSource>,

    pub data_types: FxHashMap<DatatypeSource, DataTypeId>,
    pub data_types_back: ArenaMap<DataTypeId, DatatypeSource>,

    pub structures: FxHashMap<StructureSource, StructureId>,
    pub structures_back: ArenaMap<StructureId, StructureSource>,

    pub sorts: FxHashMap<SortSource, SortId>,
    pub sorts_back: ArenaMap<SortId, SortSource>,
}

pub type ExprPtr = AstPtr<ast::Expr>;
pub type ExprSource = InFile<ExprPtr>;

pub type DedPtr = AstPtr<ast::Ded>;
pub type DedSource = InFile<DedPtr>;

pub type ModulePtr = AstPtr<ast::Module>;
pub type ModuleSource = InFile<ModulePtr>;

pub type DefinitionPtr = AstPtr<ast::MetaDefinition>;
pub type DefinitionSource = InFile<DefinitionPtr>;

pub type DatatypePtr = AstPtr<ast::Datatype>;
pub type DatatypeSource = InFile<DatatypePtr>;

pub type StructurePtr = AstPtr<ast::Structure>;
pub type StructureSource = InFile<StructurePtr>;

pub type SortPtr = AstPtr<ast::Sort>;
pub type SortSource = InFile<SortPtr>;

#[derive(PartialEq, Eq, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionSym {
    name: Name,
    scope: ScopeId,
    sort_args: Vec<Name>,
    arg_sorts: Vec<Sort>,
    ret_sort: Sort,
}

#[derive(PartialEq, Eq, Debug)]
pub enum DefKind {
    FunctionSym(FunctionSym),
    Value(PhraseId),
    Sort(SortId),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Definition {
    name: Name,
    // parent: Option<ModuleId>,
    // visibility: Visibility,
    // kind: DefKind,
}

#[derive(PartialEq, Eq, Debug)]
pub struct DataType {
    name: Name,
    // parent: Option<ModuleId>,
    constructors: Vec<DataTypeConstructor>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct DataTypeConstructor {
    name: Name,
    // parent: DataTypeId,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Structure {
    name: Name,
    // parent: Option<ModuleId>,
    constructors: Vec<DataTypeConstructor>,
}

pub type DataTypeId = Idx<DataType>;

pub type DefinitionId = Idx<Definition>;

pub type StructureId = Idx<Structure>;

pub type ModuleId = Idx<Module>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ModuleItem {
    ModuleId(ModuleId),
    DefinitionId(DefinitionId),
    DataTypeId(DataTypeId),
    StructureId(StructureId),
    PhraseId(PhraseId),
}

util::impl_from!(ModuleId, DefinitionId, DataTypeId, StructureId, PhraseId for ModuleItem);

#[derive(PartialEq, Eq, Debug)]
pub enum ModuleKind {
    Definition,
    Extension,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Module {
    pub kind: ModuleKind,
    pub name: Name,
    pub parent: Option<ModuleId>,
    // pub items: Vec<ModuleItem>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FileSema {
    pub file_hir: Arc<FileHir>,
    pub file_hir_source_map: Arc<FileHirSourceMap>,
    pub scope_tree: Arc<ScopeTree>,
}

impl FileSema {
    pub fn new(
        file_hir: Arc<FileHir>,
        file_hir_source_map: Arc<FileHirSourceMap>,
        scope_tree: Arc<ScopeTree>,
    ) -> Self {
        Self {
            file_hir,
            file_hir_source_map,
            scope_tree,
        }
    }
}

pub fn file_sema_query(db: &dyn HirDatabase, file_id: FileId) -> FileSema {
    let source_file = db.parse(file_id).tree();
    let (hir, map, scope_tree) = lower::lower(file_id, source_file);

    FileSema::new(Arc::new(hir), Arc::new(map), Arc::new(scope_tree))
}
