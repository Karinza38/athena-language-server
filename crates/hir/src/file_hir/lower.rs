#[cfg(test)]
mod tests;

use base_db::FileId;
use syntax::{
    ast::{self, HasDefineBody, HasDefineName, HasIdentifier},
    AstNode, AstPtr,
};

use crate::{
    expr::{Expr, ExprId},
    file_hir::{FileHirSourceMap, ModuleSource},
    name::{AsName, Name},
    phrase::PhraseId,
    scope::{Scope, ScopeId, ScopeKind, ScopeTree},
    sort::SortKind,
    InFile,
};

use super::{
    DataType, DataTypeId, DatatypeSource, Ded, DedId, DedSource, Definition, DefinitionId,
    DefinitionSource, ExprSource, FileHir, Module, ModuleId, ModuleItem, Sort, SortId, SortSource,
    Structure, StructureId, StructureSource,
};

struct Ctx {
    hir: FileHir,
    source_map: FileHirSourceMap,
    file_id: FileId,

    scopes: ScopeTree,

    current_module: Option<ModuleId>,
}

pub(super) fn lower(
    file_id: FileId,
    file: ast::SourceFile,
) -> (FileHir, FileHirSourceMap, ScopeTree) {
    let hir = FileHir::default();
    let source_map = FileHirSourceMap::default();

    let mut ctx = Ctx {
        hir,
        source_map,
        file_id,
        current_module: None,
        scopes: ScopeTree::new(),
    };

    let mut scope = ctx.scopes.root;

    ctx.lower_file(file, &mut scope);

    (ctx.hir, ctx.source_map, ctx.scopes)
}

macro_rules! alloc_fns {
    (impl $t: ident { $($it: ident -> $id: ident in $arena: ident),* $(,)?}) => {
		impl $t {
			$(
				paste::paste! {
					fn [<alloc_ $it:snake:lower>](&mut self, value: $it) -> $id {
						self.hir.$arena.alloc(value)
					}
				}
			)*
		}
	};
}

macro_rules! set_source_fns {
	(impl $t: ident { $($name : ident $src: ident -> $id: ident in $arena: ident),* $(,)? }) => {
		impl $t {
			$(
				paste::paste! {
					fn [<set_ $name:snake:lower _source>](&mut self, src: $src, id: $id) {
						self.source_map.$arena.insert(src.clone(), id);
						self.source_map.[<$arena _back>].insert(id, src);
					}
				}
			)*
		}
	};
}

set_source_fns!(
    impl Ctx {
        module ModuleSource -> ModuleId in modules,

        definition DefinitionSource -> DefinitionId in definitions,
        datatype DatatypeSource -> DataTypeId in data_types,
        structure StructureSource -> StructureId in structures,
        ded DedSource -> DedId in deds,
        sort SortSource -> SortId in sorts,
        expr ExprSource -> ExprId in exprs,
    }
);

alloc_fns!(
    impl Ctx {
        Module -> ModuleId in modules,

        Definition -> DefinitionId in definitions,
        DataType -> DataTypeId in data_types,
        Structure -> StructureId in structures,
        Ded -> DedId in deds,
        Sort -> SortId in sorts,
        Expr -> ExprId in exprs,
    }
);

impl Ctx {
    fn lower_file(&mut self, file: ast::SourceFile, scope: &mut ScopeId) {
        for stmt in file.stmts() {
            self.lower_stmt(stmt, scope);
        }
    }

    fn with_parent_module<T>(&mut self, module: ModuleId, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = std::mem::replace(&mut self.current_module, Some(module));

        let res = f(self);

        self.current_module = old;

        res
    }

    #[tracing::instrument(skip(self))]
    fn lower_stmt(&mut self, stmt: ast::Stmt, scope: &mut ScopeId) -> Vec<ModuleItem> {
        match stmt {
            ast::Stmt::DirStmt(dir) => self
                .lower_dir(dir.dir().unwrap(), scope)
                .into_iter()
                .flatten()
                .collect(),
            ast::Stmt::PhraseStmt(_) => todo!(),
            ast::Stmt::DatatypeStmt(_) => todo!(),
            ast::Stmt::StructureStmt(_) => todo!(),
            ast::Stmt::DatatypesStmt(_) => todo!(),
            ast::Stmt::StructuresStmt(_) => todo!(),
        }
    }

    fn make_scope(
        &mut self,
        names: Vec<Name>,
        kind: ScopeKind,
        parent: Option<ScopeId>,
    ) -> ScopeId {
        let scope = Scope {
            parent,
            introduced: names,
            kind,
        };
        self.scopes.alloc_scope(scope)
    }

    fn set_module_item_scope(&mut self, item: ModuleItem, scope: ScopeId) {
        self.scopes.set_module_item_scope(item, scope)
    }

    fn set_expr_scope(&mut self, expr: ExprId, scope: ScopeId) {
        self.scopes.set_expr_scope(expr, scope)
    }

    fn make_module_item_scope(
        &mut self,
        names: impl IntoIterator<Item = Name>,
        item: impl Into<ModuleItem> + Copy,
        parent: Option<ScopeId>,
    ) -> ScopeId {
        let scope = Scope {
            parent,
            introduced: names.into_iter().collect(),
            kind: ScopeKind::ModuleItem(item.into()),
        };
        let id = self.scopes.alloc_scope(scope);
        self.set_module_item_scope(item.into(), id);
        id
    }

    fn make_source<N: AstNode>(&self, node: &N) -> InFile<AstPtr<N>> {
        InFile::new(self.file_id, AstPtr::new(&node))
    }

    fn lower_dir(&mut self, dir: ast::Dir, scope: &mut ScopeId) -> Option<Vec<ModuleItem>> {
        match dir {
            ast::Dir::ModuleDir(module) => {
                let name = module.identifier().unwrap().as_name();

                let module_id = self.alloc_module(Module {
                    name: name.clone(),
                    kind: crate::file_hir::ModuleKind::Definition,
                    parent: self.current_module,
                });

                let mut new_scope =
                    self.make_module_item_scope(vec![name], module_id, Some(*scope));

                *scope = new_scope;

                self.with_parent_module(module_id, |ctx| {
                    for stmt in module.stmts() {
                        ctx.lower_stmt(stmt, &mut new_scope);
                    }
                });

                let module_ast = module.into();
                let src = self.make_source(&module_ast);

                self.set_module_source(src, module_id);

                Some(vec![module_id.into()])
            }
            ast::Dir::DomainDir(domain) => {
                let domain_id = self.lower_sort_decl(&domain.sort_decl()?, scope)?;
                let src = self.make_source(&ast::Domain::from(domain).into());

                self.set_definition_source(src, domain_id);

                Some(vec![domain_id.into()])
            }
            ast::Dir::DomainsDir(domains) => {
                let mut items = Vec::new();
                for sort_decl in domains.sort_decls() {
                    let domain_id = self.lower_sort_decl(&sort_decl, scope)?;

                    items.push(domain_id);
                }
                let src = self.make_source(&ast::Domain::from(domains).into());

                self.set_definition_source(src, items.last()?.clone());
                Some(items.into_iter().map(Into::into).collect())
            }
            ast::Dir::LoadDir(_) => todo!(),
            ast::Dir::AssertClosedDir(_) => todo!(),
            ast::Dir::ExtendModuleDir(_) => todo!(),
            ast::Dir::OpenDir(_) => todo!(),
            ast::Dir::AssociativityDir(_) => None,
            ast::Dir::AssertDir(_) => todo!(),
            ast::Dir::ConstantDeclareDir(_) => todo!(),
            ast::Dir::DeclareDir(_) => todo!(),
            ast::Dir::DefineDir(define) => match define {
                ast::DefineDir::InfixDefineDir(define) => self.lower_define(define, scope),
                ast::DefineDir::PrefixDefineDir(define) => match define {
                    ast::PrefixDefineDir::PrefixDefine(define) => self.lower_define(define, scope),
                    ast::PrefixDefineDir::PrefixDefineBlocks(_) => todo!(),
                },
            },
        }
    }

    fn lower_define<Def>(&mut self, define: Def, scope: &mut ScopeId) -> Option<Vec<ModuleItem>>
    where
        Def: AstNode + HasDefineName + HasDefineBody + Clone,
        ast::Definition: From<Def>,
    {
        let define_name = define.define_name()?;
        let names = match define_name {
            ast::DefineName::Identifier(id) => vec![id.as_name()],
            ast::DefineName::DefineNamedPattern(named) => {
                vec![named.identifier()?.as_name()]
            }
            ast::DefineName::DefineProc(_proc) => {
                // let args =
                // let mut names = vec![proc.identifier()?.as_name()];

                // let outer_scope = self.make_scope(names.clone(), Some(*scope));
                // let def = self.alloc_definition(Definition { name: names[0].clone() });
                // let src = self.make_source(&ast::Definition::from(define).into());
                // self.set_definition_source(src, def);
                // self.set_module_item_scope(ModuleItem::DefinitionId(def), outer_scope);
                // define.phrase()
                // names
                todo!()
            }
            ast::DefineName::ListPat(_) => todo!(),
        };

        let src = self.make_source(&ast::Definition::from(define.clone()).into());

        let mut module_items: Vec<ModuleItem> = Vec::new();
        for name in names.clone() {
            let def = self.alloc_definition(Definition { name });
            self.set_definition_source(src.clone(), def);
            module_items.push(def.into());
        }

        let new_scope =
            self.make_module_item_scope(names.clone(), module_items.last()?.clone(), Some(*scope));

        *scope = new_scope;
        self.lower_phrase(define.define_body()?, scope);

        Some(module_items)
    }

    fn lower_expr(&mut self, expr: ast::Expr, scope: &mut ScopeId) -> Option<ExprId> {
        match &expr {
            ast::Expr::IdentExpr(_)
            | ast::Expr::LiteralExpr(_)
            | ast::Expr::UnitExpr(_)
            | ast::Expr::TermVarExpr(_)
            | ast::Expr::CheckExpr(_)
            | ast::Expr::ApplicationExpr(_)
            | ast::Expr::ListExpr(_)
            | ast::Expr::TryExpr(_)
            | ast::Expr::CellExpr(_)
            | ast::Expr::SetExpr(_)
            | ast::Expr::RefExpr(_)
            | ast::Expr::WhileExpr(_)
            | ast::Expr::WildcardExpr(_)
            | ast::Expr::MakeVectorExpr(_)
            | ast::Expr::VectorSubExpr(_)
            | ast::Expr::VectorSetExpr(_)
            | ast::Expr::SeqExpr(_)
            | ast::Expr::AndExpr(_)
            | ast::Expr::OrExpr(_)
            | ast::Expr::MapExpr(_)
            | ast::Expr::PrefixCheckExpr(_)
            | ast::Expr::MatchExpr(_)
            | ast::Expr::MetaIdent(_) => {
                let id = self.alloc_expr(Expr {});
                self.set_expr_scope(id, *scope);
                let src = self.make_source(&expr);
                self.set_expr_source(src, id);
                Some(id)
            }
            ast::Expr::LambdaExpr(_) => todo!(),
            ast::Expr::MethodExpr(_) => todo!(),
            ast::Expr::LetExpr(_) => todo!(),
            ast::Expr::LetRecExpr(_) => todo!(),
        }
    }

    fn lower_phrase(&mut self, phrase: ast::Phrase, scope: &mut ScopeId) -> Option<PhraseId> {
        match phrase {
            ast::Phrase::ExprPhrase(exp) => {
                let id = self.lower_expr(exp.expr()?, scope)?;
                Some(id.into())
            }
            ast::Phrase::DedPhrase(_) => todo!(),
        }
    }

    fn lower_sort_decl(
        &mut self,
        sort_decl: &ast::SortDecl,
        scope: &mut ScopeId,
    ) -> Option<DefinitionId> {
        let name = sort_decl_name(&sort_decl)?;

        let _sort_id = self.alloc_sort(lower_sort_decl(&sort_decl)?);

        let domain_id = self.alloc_definition(Definition { name: name.clone() });

        let new_scope = self.make_scope(
            vec![name],
            ScopeKind::ModuleItem(domain_id.into()),
            Some(*scope),
        );
        self.set_module_item_scope(domain_id.into(), new_scope);

        *scope = new_scope;

        Some(domain_id)
    }
}

fn lower_sort_decl(sort_decl: &ast::SortDecl) -> Option<Sort> {
    match sort_decl {
        ast::SortDecl::IdentSort(ident) => Some(Sort {
            kind: SortKind::Ident(ident.identifier()?.as_name()),
        }),
        ast::SortDecl::CompoundSortDecl(compound) => Some(Sort {
            kind: SortKind::Compound(
                compound
                    .ident_sorts()
                    .filter_map(|ident| {
                        let decl = ident.into();
                        lower_sort_decl(&decl)
                    })
                    .collect(),
            ),
        }),
    }
}

fn sort_decl_name(sort_decl: &ast::SortDecl) -> Option<Name> {
    match sort_decl {
        ast::SortDecl::IdentSort(ident) => Some(ident.identifier()?.as_name()),
        ast::SortDecl::CompoundSortDecl(compound) => compound
            .ident_sorts()
            .next()?
            .identifier()
            .map(|ident| ident.as_name()),
    }
}
