use base_db::FileId;
use core::fmt::Debug;
use syntax::{ast, match_ast, AstNode, AstPtr};

use crate::{
    expr::ExprId,
    file_hir::ModuleItem,
    name::{AsName, Name},
    pat::PatId,
    scope::{Scope, ScopeId, ScopeKind},
    FileSema, HasHir, HirDatabase, HirNode, InFile,
};

pub struct Semantics<'db, DB> {
    pub db: &'db DB,
}

impl<'db, DB> Semantics<'db, DB>
where
    DB: HirDatabase,
{
    pub fn new(db: &'db DB) -> Self {
        Self { db }
    }

    pub fn parse(&self, file_id: FileId) -> ast::SourceFile {
        self.db.parse(file_id).tree()
    }

    pub fn file_sema(&self, file_id: FileId) -> FileSema {
        self.db.file_sema(file_id)
    }

    pub fn resolve_name_ref(&self, name: InFile<ast::NameRef>) -> Option<NameRefResolution> {
        let InFile {
            file_id,
            value: name,
        } = name;

        let file_sema = self.db.file_sema(file_id);
        let resolvable = find_resolvable_node(&name)?;
        let scope = resolvable.scope(&file_sema)?;

        let def_scope = find_defining_scope(&name.as_name(), scope, &file_sema)?;

        let resolution = NameRefResolution::from(def_scope.kind.clone());

        Some(resolution)
    }

    pub fn get_source_for<N>(&self, hir: InFile<N::Id>) -> Option<N::Source>
    where
        N: HirNode,
    {
        let InFile {
            file_id,
            value: hir,
        } = hir;
        let file_sema = self.db.file_sema(file_id);
        N::source(hir, &file_sema.file_hir_source_map)
    }

    pub fn get_hir_for<S>(&self, source: S) -> Option<<S::Hir as HirNode>::Id>
    where
        S: HasHir,
    {
        let file_id = source.file_id();
        let file_sema = self.db.file_sema(file_id);
        source.hir(&file_sema.file_hir_source_map)
    }

    pub fn get_scope_for<N>(&self, hir: InFile<N::Id>) -> Option<ScopeId>
    where
        N: HirNode,
    {
        let InFile {
            file_id,
            value: hir,
        } = hir;
        let file_sema = self.db.file_sema(file_id);
        N::scope(hir, &file_sema.scope_tree)
    }

    pub fn get_scope_for_source<S>(&self, source: S) -> Option<ScopeId>
    where
        S: HasHir,
        S::Hir: HirNode,
    {
        let file_id = source.file_id();
        let file_sema = self.db.file_sema(file_id);

        file_sema
            .get_hir_for(source)
            .and_then(|hir| S::Hir::scope(hir, &file_sema.scope_tree))
    }
}

enum Resolvable {
    Expr(ast::Expr),
    Sort(ast::Sort),
}

impl Resolvable {
    fn scope<'a>(&self, sema: &'a FileSema) -> Option<&'a Scope> {
        match self {
            Resolvable::Expr(expr) => node_to_scope(expr, sema),
            Resolvable::Sort(sort) => node_to_scope(sort, sema),
        }
    }
}

#[tracing::instrument(skip(sema))]
fn node_to_scope<'n, 'a, N: AstNode + Debug>(node: &'n N, sema: &'a FileSema) -> Option<&'a Scope>
where
    InFile<AstPtr<N>>: HasHir,
{
    let source = InFile::new(sema.file_id, AstPtr::new(node));
    Some(sema.scope(sema.get_scope_for_source(source)?))
}

fn find_resolvable_node(name_ref: &ast::NameRef) -> Option<Resolvable> {
    let mut node = name_ref.syntax().parent();
    while let Some(parent) = node {
        match_ast! {
            match parent {
                ast::Expr(it) => {
                    return Some(Resolvable::Expr(it));
                },
                ast::Sort(it) => {
                    return Some(Resolvable::Sort(it));
                },
                _ => {}
            }
        }
        node = parent.parent();
    }
    None
}

fn find_defining_scope<'s>(
    name: &Name,
    start_scope: &'s Scope,
    sema: &'s FileSema,
) -> Option<&'s Scope> {
    let mut scope = start_scope;
    tracing::info!(?scope, "finding defining scope");
    while scope.kind != ScopeKind::Root {
        tracing::info!(?scope, "considering scope");
        if scope.introduced.contains(name) {
            return Some(scope);
        }
        scope = sema.scope_tree.scope(scope.parent?);
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameRefResolution {
    ModuleItem(ModuleItem),
    Local(Local),
}

impl From<ScopeKind> for NameRefResolution {
    fn from(value: ScopeKind) -> Self {
        match value {
            ScopeKind::Expr(expr) => NameRefResolution::Local(Local::Expr(expr)),
            ScopeKind::Pat(pat) => NameRefResolution::Local(Local::Pat(pat)),
            ScopeKind::ModuleItem(mi) => NameRefResolution::ModuleItem(mi),
            ScopeKind::Root => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Local {
    Pat(PatId),
    Expr(ExprId),
}
