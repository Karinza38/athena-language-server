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
    identifier::{Identifier, IdentifierId},
    name::Name,
    phrase::PhraseId,
    scope::{Scope, ScopeId, ScopeTree},
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
    pub identifiers: Arena<Identifier>,
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

    pub identifiers: FxHashMap<IdentifierSource, IdentifierId>,
    pub identifiers_back: ArenaMap<IdentifierId, IdentifierSource>,
}

pub type IdentifierPtr = AstPtr<ast::Identifier>;
pub type IdentifierSource = InFile<IdentifierPtr>;

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
    pub file_id: FileId,
    pub file_hir: Arc<FileHir>,
    pub file_hir_source_map: Arc<FileHirSourceMap>,
    pub scope_tree: Arc<ScopeTree>,
}

impl FileSema {
    pub fn new(
        file_id: FileId,
        file_hir: Arc<FileHir>,
        file_hir_source_map: Arc<FileHirSourceMap>,
        scope_tree: Arc<ScopeTree>,
    ) -> Self {
        Self {
            file_id,
            file_hir,
            file_hir_source_map,
            scope_tree,
        }
    }

    pub fn get_source_for<N>(&self, hir: N::Id) -> Option<N::Source>
    where
        N: HirNode,
    {
        N::source(hir, &self.file_hir_source_map)
    }

    pub fn get_hir_for<S>(&self, source: S) -> Option<<S::Hir as HirNode>::Id>
    where
        S: HasHir,
    {
        source.hir(&self.file_hir_source_map)
    }

    pub fn get_scope_for<N>(&self, hir: N::Id) -> Option<ScopeId>
    where
        N: HirNode,
    {
        N::scope(hir, &self.scope_tree)
    }

    pub fn get_scope_for_source<S>(&self, source: S) -> Option<ScopeId>
    where
        S: HasHir,
        S::Hir: HirNode,
    {
        self.get_hir_for(source)
            .and_then(|hir| S::Hir::scope(hir, &self.scope_tree))
    }

    pub fn scope(&self, scope_id: ScopeId) -> &Scope {
        self.scope_tree.scope(scope_id)
    }
}

pub fn file_sema_query(db: &dyn HirDatabase, file_id: FileId) -> FileSema {
    let source_file = db.parse(file_id).tree();
    let (hir, map, scope_tree) = lower::lower(file_id, source_file);

    FileSema::new(file_id, Arc::new(hir), Arc::new(map), Arc::new(scope_tree))
}

pub trait HasHir {
    type Hir: HirNode;

    fn hir(&self, source_map: &FileHirSourceMap) -> Option<<Self::Hir as HirNode>::Id>;
}

pub trait HirNode {
    type Source: HasHir + HasSyntaxNodePtr;
    type Id: Copy;

    fn source(id: Self::Id, source_map: &FileHirSourceMap) -> Option<Self::Source>;

    fn scope(id: Self::Id, scope_tree: &ScopeTree) -> Option<ScopeId>;
}

pub trait HasSyntaxNodePtr {
    fn syntax_node_ptr(&self) -> syntax::SyntaxNodePtr;
}

impl<N: syntax::AstNode> HasSyntaxNodePtr for InFile<AstPtr<N>> {
    fn syntax_node_ptr(&self) -> syntax::SyntaxNodePtr {
        self.value.syntax_node_ptr()
    }
}

impl<N: syntax::AstNode> HasSyntaxNodePtr for N {
    fn syntax_node_ptr(&self) -> syntax::SyntaxNodePtr {
        syntax::SyntaxNodePtr::new(self.syntax())
    }
}

duplicate::duplicate! {
    [
        id_type         hir_type        source_type         arena_name      scope_lookup            ;
        [ModuleId]      [Module]        [ModuleSource]      [modules]       [scope_by_module_item]        ;
        [DefinitionId]  [Definition]    [DefinitionSource]  [definitions]   [scope_by_module_item] ;
        // [DataTypeId]    [DataType]      [DatatypeSource]    [datatypes];
        [StructureId]   [Structure]     [StructureSource]   [structures]    [scope_by_module_item] ;
        [ExprId]        [Expr]          [ExprSource]        [exprs]         [scope_by_expr]        ;
        [SortId]        [Sort]          [SortSource]        [sorts]         [scope_by_sort]        ;
        [IdentifierId]  [Identifier]    [IdentifierSource]  [identifiers]   [scope_by_identifier]  ;
        [DedId]         [Ded]           [DedSource]         [deds]          [scope_by_ded]         ;
    ]
    impl HirNode for hir_type {
        type Source = source_type;
        type Id = id_type;

        fn source(id: id_type, source_map: &FileHirSourceMap) -> Option<Self::Source> {
            paste::paste! {
                source_map.[<arena_name _back>].get(id).cloned()
            }
        }

        fn scope(id: id_type, scope_tree: &ScopeTree) -> Option<ScopeId> {
            #[allow(clippy::useless_conversion)]
            scope_tree.scope_lookup(id.into())
        }
    }
    impl HasHir for source_type {
        type Hir = hir_type;

        fn hir(&self, source_map: &FileHirSourceMap) -> Option<id_type> {
            source_map.arena_name.get(self).cloned()
        }
    }
}
