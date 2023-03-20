use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;

use crate::{
    ded::DedId, expr::ExprId, file_hir::ModuleItem, identifier::IdentifierId, name::Name,
    sort::SortId,
};

#[derive(PartialEq, Eq, Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub introduced: Vec<Name>,
    pub kind: ScopeKind,
}

pub type ScopeId = Idx<Scope>;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum ScopeKind {
    Expr(ExprId),
    ModuleItem(ModuleItem),
    Root,
}

impl From<ExprId> for ScopeKind {
    fn from(expr: ExprId) -> Self {
        Self::Expr(expr)
    }
}

impl<T> From<T> for ScopeKind
where
    T: Into<ModuleItem>,
{
    fn from(value: T) -> Self {
        Self::ModuleItem(value.into())
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ScopeTree {
    scopes: Arena<Scope>,

    pub root: ScopeId,

    scopes_by_expr: ArenaMap<ExprId, ScopeId>,
    scopes_by_ded: ArenaMap<DedId, ScopeId>,
    scopes_by_module_item: FxHashMap<ModuleItem, ScopeId>,
    scopes_by_sort: ArenaMap<SortId, ScopeId>,

    scopes_by_identifier: ArenaMap<IdentifierId, ScopeId>,
}

impl ScopeTree {
    pub fn new() -> Self {
        let mut scopes = Arena::default();
        let root = scopes.alloc(Scope {
            parent: None,
            introduced: Vec::new(),
            kind: ScopeKind::Root,
        });
        Self {
            scopes,
            root,
            scopes_by_expr: ArenaMap::default(),
            scopes_by_ded: ArenaMap::default(),
            scopes_by_module_item: FxHashMap::default(),
            scopes_by_sort: ArenaMap::default(),
            scopes_by_identifier: ArenaMap::default(),
        }
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id]
    }

    pub fn set_expr_scope(&mut self, expr: ExprId, scope: ScopeId) {
        self.scopes_by_expr.insert(expr, scope);
    }

    pub fn set_ded_scope(&mut self, ded: DedId, scope: ScopeId) {
        self.scopes_by_ded.insert(ded, scope);
    }

    pub fn set_sort_scope(&mut self, sort: SortId, scope: ScopeId) {
        self.scopes_by_sort.insert(sort, scope);
    }

    pub fn set_module_item_scope(&mut self, item: ModuleItem, scope: ScopeId) {
        self.scopes_by_module_item.insert(item, scope);
    }

    pub fn set_identifier_scope(&mut self, identifier: IdentifierId, scope: ScopeId) {
        self.scopes_by_identifier.insert(identifier, scope);
    }

    pub fn scope_by_expr(&self, id: ExprId) -> Option<ScopeId> {
        self.scopes_by_expr.get(id).copied()
    }

    pub fn scope_by_ded(&self, id: DedId) -> Option<ScopeId> {
        self.scopes_by_ded.get(id).copied()
    }

    pub fn scope_by_module_item(&self, id: ModuleItem) -> Option<ScopeId> {
        self.scopes_by_module_item.get(&id).copied()
    }

    pub fn scope_by_sort(&self, id: SortId) -> Option<ScopeId> {
        self.scopes_by_sort.get(id).copied()
    }

    pub fn scope_by_identifier(&self, id: IdentifierId) -> Option<ScopeId> {
        self.scopes_by_identifier.get(id).copied()
    }

    pub fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        let id = self.scopes.alloc(scope);
        match &self.scopes[id].kind {
            ScopeKind::Expr(expr) => {
                self.scopes_by_expr.insert(*expr, id);
            }
            ScopeKind::ModuleItem(item) => {
                self.scopes_by_module_item.insert(*item, id);
            }
            ScopeKind::Root => {}
        }
        id
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}
