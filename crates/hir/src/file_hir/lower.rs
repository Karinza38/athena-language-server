#[cfg(test)]
mod tests;

use base_db::FileId;
use syntax::{
    ast::{
        self, AstChildren, HasDefineBody, HasDefineName, HasIdentifier, MaybeWildcardTypedParam,
    },
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
                    #[allow(dead_code)]
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
                    #[allow(dead_code)]
					fn [<set_ $name:snake:lower _source>](&mut self, src: $src, id: $id) {
                        self.source_map.$arena.insert(src.clone(), id);
						self.source_map.[<$arena _back>].insert(id, src);
					}

                    #[allow(dead_code)]
                    fn [<set_ $name:snake:lower _source_fw>](&mut self, src: $src, id: $id) {
                        self.source_map.$arena.insert(src.clone(), id);
					}

                    #[allow(dead_code)]
                    fn [<set_ $name:snake:lower _source_bw>](&mut self, src: $src, id: $id) {
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

macro_rules! or_continue {
    ($exp: expr) => {
        match $exp {
            Some(it) => it,
            None => continue,
        }
    };
}

macro_rules! or_return {
    ($exp: expr) => {
        match $exp {
            Some(it) => it,
            None => return,
        }
    };
    ($exp: expr, default) => {
        match $exp {
            Some(it) => it,
            None => return Default::default(),
        }
    };
    ($exp: expr, $ret: expr) => {
        match $exp {
            Some(it) => it,
            None => return $ret,
        }
    };
}

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
            ast::Stmt::PhraseStmt(p) => self
                .lower_phrase(or_return!(p.phrase(), vec![]), scope)
                .into_iter()
                .map(Into::into)
                .collect(),
            ast::Stmt::DatatypeStmt(_) => {
                // todo!(); TODO: implement
                Vec::new()
            }
            ast::Stmt::StructureStmt(_) => {
                // todo!(); TODO: implement
                Vec::new()
            }
            ast::Stmt::DatatypesStmt(_) => {
                // todo!(); TODO: implement
                Vec::new()
            }
            ast::Stmt::StructuresStmt(_) => {
                // todo!(); TODO: implement
                Vec::new()
            }
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

    fn set_ded_scope(&mut self, ded: DedId, scope: ScopeId) {
        self.scopes.set_ded_scope(ded, scope)
    }

    fn set_sort_scope(&mut self, sort: SortId, scope: ScopeId) {
        self.scopes.set_sort_scope(sort, scope)
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
                let src = self.make_source(&ast::Domain::from(domain.clone()).into());
                let domain_id = self.lower_sort_decl(&domain.sort_decl()?, scope, src.clone())?;

                self.set_definition_source(src, domain_id);

                Some(vec![domain_id.into()])
            }
            ast::Dir::DomainsDir(domains) => {
                let src = self.make_source(&ast::Domain::from(domains.clone()).into());
                let mut items = Vec::new();
                for sort_decl in domains.sort_decls() {
                    let domain_id = self.lower_sort_decl(&sort_decl, scope, src.clone())?;

                    items.push(domain_id);
                }

                Some(items.into_iter().map(Into::into).collect())
            }
            ast::Dir::LoadDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::AssertClosedDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::ExtendModuleDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::OpenDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::AssociativityDir(_) => None,
            ast::Dir::AssertDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::ConstantDeclareDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::DeclareDir(declare) => match declare {
                ast::DeclareDir::PrefixDeclareDir(_) => {
                    // todo!(); TODO: implement
                    None
                }
                ast::DeclareDir::InfixDeclareDir(declare) => {
                    let names: Vec<Name> =
                        declare.identifiers().map(|ident| ident.as_name()).collect();
                    let mut defs = Vec::new();
                    for name in &names {
                        defs.push(self.alloc_definition(Definition { name: name.clone() }));
                    }
                    let new_scope = self.make_scope(
                        names,
                        ScopeKind::ModuleItem(ModuleItem::DefinitionId(*defs.last()?)),
                        Some(*scope),
                    );

                    *scope = new_scope;

                    if let Some(sorts) = declare.func_sorts() {
                        for sort in sorts.sorts() {
                            self.lower_sort(sort, scope);
                        }
                    }

                    self.lower_sort_opt(declare.return_sort(), scope);

                    Some(defs.into_iter().map(Into::into).collect())
                }
            },
            ast::Dir::DefineDir(define) => match define {
                ast::DefineDir::InfixDefineDir(define) => self.lower_define(define, scope),
                ast::DefineDir::PrefixDefineDir(define) => match define {
                    ast::PrefixDefineDir::PrefixDefine(define) => self.lower_define(define, scope),
                    ast::PrefixDefineDir::PrefixDefineBlocks(_) => {
                        // todo!(); TODO: implement
                        None
                    }
                },
            },
            ast::Dir::SetPrecedenceDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::OverloadDir(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Dir::RuleDir(_) => {
                // todo!(); TODO: implement
                None
            }
        }
    }

    fn lower_sort_opt(&mut self, sort: Option<ast::Sort>, scope: &mut ScopeId) -> Option<SortId> {
        sort.map(|sort| self.lower_sort(sort, scope)).flatten()
    }

    #[tracing::instrument(skip(self, define))]
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
            ast::DefineName::DefineProc(proc) => {
                // let args =
                let name = proc.identifier()?.as_name();
                let def = self.alloc_definition(Definition { name: name.clone() });

                let mut outer_scope =
                    self.make_scope(vec![name], ScopeKind::ModuleItem(def.into()), Some(*scope));
                let src = self.make_source(&ast::Definition::from(define.clone()).into());
                self.set_definition_source(src, def);
                self.set_module_item_scope(ModuleItem::DefinitionId(def), outer_scope);

                let args = self.lower_args(proc.args(), &mut outer_scope);

                let mut inner_scope = self.make_scope(
                    args.clone(),
                    ScopeKind::ModuleItem(def.into()),
                    Some(outer_scope),
                );

                let _body = self.lower_phrase(define.define_body()?, &mut inner_scope);

                *scope = outer_scope;

                return Some(vec![def.into()]);
            }
            ast::DefineName::ListPat(_) => {
                // todo!(); TODO: implement
                Vec::new()
            }
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

                walk_child_exprs(expr, |exp| {
                    self.lower_expr(exp, scope);
                });

                Some(id)
            }
            ast::Expr::LambdaExpr(lambda) => {
                let arg_names = self.lower_args(lambda.maybe_wildcard_typed_params(), scope);

                let lambda_expr = self.alloc_expr(Expr {});

                let mut body_scope = self.make_scope(
                    arg_names.clone(),
                    ScopeKind::Expr(lambda_expr),
                    Some(*scope),
                );

                let body = self.lower_expr(lambda.expr()?, &mut body_scope)?;

                let src = self.make_source(&expr);
                self.set_expr_source(src, lambda_expr);
                self.set_expr_scope(lambda_expr, *scope);

                let _ = body; //TODO: model lambda and set the body here
                Some(lambda_expr)
            }
            ast::Expr::MethodExpr(method) => {
                let arg_names = self.lower_args(method.maybe_wildcard_typed_params(), scope);

                let method_expr = self.alloc_expr(Expr {});

                let mut body_scope = self.make_scope(
                    arg_names.clone(),
                    ScopeKind::Expr(method_expr),
                    Some(*scope),
                );

                let body = self.lower_ded(method.ded()?, &mut body_scope)?;

                let src = self.make_source(&expr);
                self.set_expr_source(src, method_expr);
                self.set_expr_scope(method_expr, *scope);

                let _ = body; //TODO: model method and set the body here
                Some(method_expr)
            }
            ast::Expr::LetExpr(_) => {
                // todo!(); TODO: implement
                None
            }
            ast::Expr::LetRecExpr(_) => {
                // todo!(); TODO: implement
                None
            }
        }
    }

    fn lower_phrase(&mut self, phrase: ast::Phrase, scope: &mut ScopeId) -> Option<PhraseId> {
        match phrase {
            ast::Phrase::ExprPhrase(exp) => {
                let id = self.lower_expr(exp.expr()?, scope)?;
                Some(id.into())
            }
            ast::Phrase::DedPhrase(d) => {
                let id = self.lower_ded(d.ded()?, scope)?;
                Some(id.into())
            }
        }
    }

    fn lower_ded(&mut self, ded: ast::Ded, scope: &mut ScopeId) -> Option<DedId> {
        walk_child_exprs(ded.clone(), |expr| {
            self.lower_expr(expr, scope);
        });

        let id = self.alloc_ded(Ded {});
        let src = self.make_source(&ded);
        self.set_ded_source(src, id);
        self.set_ded_scope(id, *scope);

        Some(id)
    }

    fn lower_sort_decl(
        &mut self,
        sort_decl: &ast::SortDecl,
        scope: &mut ScopeId,
        src: DefinitionSource,
    ) -> Option<DefinitionId> {
        let name = sort_decl_name(&sort_decl)?;

        let domain_id = self.alloc_definition(Definition { name: name.clone() });

        let new_scope = self.make_scope(
            vec![name],
            ScopeKind::ModuleItem(domain_id.into()),
            Some(*scope),
        ); // FIXME: this is wrong, there should be a shared scope for all the introduced sorts
        self.set_module_item_scope(domain_id.into(), new_scope);
        self.set_definition_source_bw(src, domain_id);
        *scope = new_scope;

        let _sort_id = self.lower_sort(sort_decl.clone().into(), scope)?;

        Some(domain_id)
    }

    fn lower_sort(&mut self, sort: ast::Sort, scope: &mut ScopeId) -> Option<SortId> {
        let id = match sort.clone() {
            ast::Sort::VarSort(v) => {
                let v = v.identifier()?.as_name();
                self.alloc_sort(Sort {
                    kind: SortKind::Var(v),
                })
            }
            ast::Sort::IdentSort(ident) => {
                let id = ident.identifier()?.as_name();
                self.alloc_sort(Sort {
                    kind: SortKind::Ident(id),
                })
            }
            ast::Sort::CompoundSort(compound) => {
                let mut sorts = Vec::new();
                for s in compound.sorts() {
                    if let Some(sort_id) = self.lower_sort(s, scope) {
                        sorts.push(sort_id);
                    }
                }
                self.alloc_sort(Sort {
                    kind: SortKind::Compound(sorts),
                })
            }
        };

        let src = self.make_source(&sort);
        self.set_sort_source(src, id);
        self.set_sort_scope(id, *scope);

        Some(id)
    }

    fn lower_args(
        &mut self,
        args: AstChildren<MaybeWildcardTypedParam>,
        scope: &mut ScopeId,
    ) -> Vec<Name> {
        let mut arg_names = Vec::new();
        for p in args {
            let arg = match p {
                ast::MaybeWildcardTypedParam::MaybeTypedParam(p) => match p {
                    ast::MaybeTypedParam::Identifier(id) => id.as_name(),
                    ast::MaybeTypedParam::TypedParam(tp) => {
                        if let Some(sort) = tp.sort() {
                            self.lower_sort(sort, scope);
                        }
                        or_continue!(tp.identifier()).as_name()
                    }
                    ast::MaybeTypedParam::OpAnnotatedParam(op) => {
                        or_continue!(op.identifier()).as_name()
                    }
                },
                ast::MaybeWildcardTypedParam::Wildcard(_) => continue,
            };
            arg_names.push(arg);
        }
        arg_names
    }
}

fn sort_decl_name(sort_decl: &ast::SortDecl) -> Option<Name> {
    match sort_decl {
        ast::SortDecl::IdentSort(ident) => Some(ident.identifier()?.as_name()),
        ast::SortDecl::CompoundSort(compound) => compound
            .sorts()
            .next()?
            .as_ident_sort()?
            .identifier()
            .map(|ident| ident.as_name()),
    }
}

trait HasChildExprs: AstNode {}

impl HasChildExprs for ast::Expr {}
impl HasChildExprs for ast::Ded {}

fn walk_child_exprs<N: HasChildExprs>(node: N, mut f: impl FnMut(ast::Expr)) {
    let mut preorder = node.syntax().preorder();
    let nd = preorder.next(); // skip the node itself
    assert!(matches!(nd, Some(syntax::WalkEvent::Enter(n)) if &n == node.syntax()));
    while let Some(event) = preorder.next() {
        match event {
            syntax::WalkEvent::Enter(nd) => {
                if let Some(expr) = ast::Expr::cast(nd) {
                    f(expr);
                    preorder.skip_subtree();
                }
            }
            syntax::WalkEvent::Leave(_) => {}
        }
    }
}
