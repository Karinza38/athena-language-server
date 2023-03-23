#[cfg(test)]
mod tests;

use base_db::FileId;
use la_arena::Idx;
use paste::paste;
use syntax::{
    ast::{
        self, AstChildren, HasDefineBody, HasDefineName, HasName, HasNameRef,
        MaybeWildcardTypedParam,
    },
    AstNode, AstPtr, AstToken,
};

use crate::{
    expr::{Expr, ExprId},
    file_hir::{FileHirSourceMap, ModuleSource},
    name::{AsName, Name},
    name_ref::NameRefId,
    pat::{Pat, PatId, PatKind},
    phrase::PhraseId,
    scope::{Scope, ScopeId, ScopeKind, ScopeTree},
    sort::SortKind,
    HasHir, HirNode, InFile,
};

use super::{
    DataType, DataTypeId, DatatypeSource, Ded, DedId, DedSource, DefKind, Definition, DefinitionId,
    DefinitionSource, ExprSource, FileHir, Module, ModuleId, ModuleItem, NameRefSource, PatSource,
    Sort, SortId, SortSource, Structure, StructureId, StructureSource,
};

struct Ctx {
    hir: FileHir,
    source_map: FileHirSourceMap,
    file_id: FileId,

    scopes: ScopeTree,

    current_module: Option<ModuleId>,

    scope_stack: Vec<ScopeId>,
}

pub(super) fn lower(
    file_id: FileId,
    file: ast::SourceFile,
) -> (FileHir, FileHirSourceMap, ScopeTree) {
    let hir = FileHir::default();
    let source_map = FileHirSourceMap::default();

    let scopes = ScopeTree::new();
    let root = scopes.root;

    let mut ctx = Ctx {
        hir,
        source_map,
        file_id,
        current_module: None,
        scopes,
        scope_stack: vec![root],
    };

    ctx.lower_file(file);

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
        pat PatSource -> PatId in pats,
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
        Pat -> PatId in pats,
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

struct HirBuilder<N> {
    file_id: FileId,
    ast: N,
    scope: Option<ScopeId>,
    introduces: Option<Vec<Name>>,
}

impl<N> HirBuilder<N> {
    fn new(ctx: &Ctx, ast: N) -> Self {
        HirBuilder {
            file_id: ctx.file_id,
            ast,
            scope: None,
            introduces: None,
        }
    }

    #[allow(dead_code)]
    fn with_scope(mut self, scope: ScopeId) -> Self {
        self.scope = Some(scope);
        self
    }
}

type SourceOf<N> = InFile<AstPtr<N>>;

type HirOf<N> = <SourceOf<N> as HasHir>::Hir;

type HirIdOf<N> = <<SourceOf<N> as HasHir>::Hir as HirNode>::Id;

impl<N> HirBuilder<N>
where
    N: AstNode,
    SourceOf<N>: HasHir,
    HirIdOf<N>: WithScope,
    HirOf<N>: MakeNode,
    <SourceOf<N> as HasHir>::Hir: HirNode<Id = Idx<HirOf<N>>>,
    HirIdOf<N>: WithSource<Source = SourceOf<N>>,
{
    fn build(self, ctx: &mut Ctx, hir: HirOf<N>) -> HirIdOf<N> {
        let HirBuilder {
            file_id,
            ast,
            scope,
            introduces: _,
        } = self;

        let ptr = AstPtr::new(&ast);

        let source = InFile {
            file_id,
            value: ptr,
        };

        let hir_id = hir.make_node(&mut ctx.hir);

        hir_id.set_source(source, &mut ctx.source_map);

        let scope = scope.unwrap_or_else(|| ctx.current_scope());

        hir_id.set_scope(scope, &mut ctx.scopes);

        hir_id
    }
}

impl<N> HirBuilder<N>
where
    N: AstNode,
    SourceOf<N>: HasHir,
    HirIdOf<N>: WithScope,
    HirOf<N>: MakeNode,
    HirIdOf<N>: BindingItem,
    <SourceOf<N> as HasHir>::Hir: HirNode<Id = Idx<HirOf<N>>>,
    HirIdOf<N>: WithSource<Source = SourceOf<N>>,
{
    fn introduces(self, names: impl IntoIterator<Item = Name>) -> Self {
        HirBuilder {
            introduces: Some(names.into_iter().collect()),
            ..self
        }
    }
    fn build_binding(self, ctx: &mut Ctx, hir: HirOf<N>) -> (HirIdOf<N>, ScopeId) {
        let HirBuilder {
            file_id,
            ast,
            scope,
            introduces,
        } = self;

        let introduces = introduces.unwrap_or_default();

        let ptr = AstPtr::new(&ast);

        let source = InFile {
            file_id,
            value: ptr,
        };

        let hir_id = hir.make_node(&mut ctx.hir);

        let current_scope = scope.unwrap_or_else(|| ctx.current_scope());

        let new_scope = ctx.alloc_scope(hir_id.make_scope(introduces, Some(current_scope)));

        hir_id.set_source(source, &mut ctx.source_map);

        hir_id.set_scope(current_scope, &mut ctx.scopes);

        (hir_id, new_scope)
    }
}

impl Ctx {
    fn lower_file(&mut self, file: ast::SourceFile) {
        for stmt in file.stmts() {
            self.lower_stmt(stmt);
        }
    }

    fn with_parent_module<T>(&mut self, module: ModuleId, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = std::mem::replace(&mut self.current_module, Some(module));

        let res = f(self);

        self.current_module = old;

        res
    }

    #[tracing::instrument(skip(self))]
    fn lower_stmt(&mut self, stmt: ast::Stmt) -> Vec<ModuleItem> {
        match stmt {
            ast::Stmt::DirStmt(dir) => self
                .lower_dir(dir.dir().unwrap())
                .into_iter()
                .flatten()
                .collect(),
            ast::Stmt::PhraseStmt(p) => self
                .lower_phrase(or_return!(p.phrase(), vec![]))
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

    fn make_scope(&mut self, names: Vec<Name>, kind: ScopeKind) -> ScopeId {
        let scope = Scope {
            parent: Some(self.current_scope()),
            introduced: names,
            kind,
        };
        self.scopes.alloc_scope(scope)
    }

    fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        self.scopes.alloc_scope(scope)
    }

    fn set_module_item_scope(&mut self, item: ModuleItem, scope: ScopeId) {
        self.scopes.set_module_item_scope(item, scope)
    }

    fn make_module_item_scope(
        &mut self,
        names: impl IntoIterator<Item = Name>,
        item: impl Into<ModuleItem> + Copy,
    ) -> ScopeId {
        let scope = Scope {
            parent: Some(self.current_scope()),
            introduced: names.into_iter().collect(),
            kind: ScopeKind::ModuleItem(item.into()),
        };
        let id = self.scopes.alloc_scope(scope);
        self.set_module_item_scope(item.into(), id);
        id
    }

    fn make_source<N: AstNode>(&self, node: &N) -> SourceOf<N> {
        InFile::new(self.file_id, AstPtr::new(node))
    }

    fn current_scope(&self) -> ScopeId {
        self.scope_stack
            .last()
            .copied()
            .expect("we never pop the root scope")
    }

    fn push_scope(&mut self, scope: ScopeId) {
        self.scope_stack.push(scope)
    }

    fn pop_scope(&mut self) -> ScopeId {
        self.scope_stack.pop().expect("we never pop the root scope")
    }

    fn with_scope<R>(&mut self, scope: ScopeId, f: impl FnOnce(&mut Self) -> R) -> R {
        self.push_scope(scope);
        let ret = f(self);
        self.pop_scope();
        ret
    }

    fn builder<N>(&self, ast: N) -> HirBuilder<N> {
        HirBuilder::new(self, ast)
    }

    fn lower_dir(&mut self, dir: ast::Dir) -> Option<Vec<ModuleItem>> {
        match dir {
            ast::Dir::ModuleDir(module) => {
                let name = module.name().unwrap().as_name();

                let module_id = self.alloc_module(Module {
                    name: name.clone(),
                    kind: crate::file_hir::ModuleKind::Definition,
                    parent: self.current_module,
                });

                let new_scope = self.make_module_item_scope(vec![name], module_id);

                self.with_scope(new_scope, |ctx| {
                    ctx.with_parent_module(module_id, |ctx| {
                        for stmt in module.stmts() {
                            ctx.lower_stmt(stmt);
                        }
                    });
                });

                let module_ast = module.into();
                let src = self.make_source(&module_ast);

                self.set_module_source(src, module_id);

                Some(vec![module_id.into()])
            }
            ast::Dir::DomainDir(domain) => {
                let src = self.make_source(&ast::Domain::from(domain.clone()).into());
                let domain_id = self.lower_sort_decl(&domain.sort_decl()?, src.clone())?;

                self.set_definition_source(src, domain_id);

                Some(vec![domain_id.into()])
            }
            ast::Dir::DomainsDir(domains) => {
                let src = self.make_source(&ast::Domain::from(domains.clone()).into());
                let mut items = Vec::new();
                for sort_decl in domains.sort_decls() {
                    let domain_id = self.lower_sort_decl(&sort_decl, src.clone())?;

                    items.push(domain_id);
                }

                Some(items.into_iter().map(Into::into).collect())
            }
            ast::Dir::LoadDir(_) => {
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
            ast::Dir::AssertClosedDir(assert) => {
                if let Some(expr) = assert.expr() {
                    let _ = self.lower_expr(expr);
                }
                if let Some(name) = assert.name() {
                    let name = name.as_name();
                    let (def, new_scope) = self
                        .builder::<ast::MetaDefinition>(ast::Assertion::from(assert).into())
                        .introduces(Some(name.clone()))
                        .build_binding(
                            self,
                            Definition {
                                name,
                                kind: DefKind::Value,
                            },
                        );
                    self.push_scope(new_scope);
                    Some(vec![def.into()])
                } else {
                    None
                }
            }
            ast::Dir::AssertDir(assert) => {
                let exprs = match &assert {
                    ast::AssertDir::InfixAssertDir(infix) => infix.exprs(),
                    ast::AssertDir::PrefixAssertDir(prefix) => prefix.exprs(),
                };
                for expr in exprs {
                    let _ = self.lower_expr(expr);
                }
                if let Some(name) = assert.name() {
                    let name = name.as_name();
                    let (def, new_scope) = self
                        .builder::<ast::MetaDefinition>(ast::Assertion::from(assert).into())
                        .introduces(Some(name.clone()))
                        .build_binding(
                            self,
                            Definition {
                                name,
                                kind: DefKind::Value,
                            },
                        );
                    self.push_scope(new_scope);
                    Some(vec![def.into()])
                } else {
                    None
                }
            }
            ast::Dir::ConstantDeclareDir(declare) => {
                match declare {
                    ast::ConstantDeclareDir::InfixConstantDeclare(declare) => {
                        let names: Vec<Name> =
                            declare.names().map(|ident| ident.as_name()).collect();
                        let mut defs = Vec::new();
                        for name in &names {
                            let def = self
                                .builder::<ast::MetaDefinition>(
                                    ast::FunctionSymbol::from(ast::ConstantDeclareDir::from(
                                        declare.clone(),
                                    ))
                                    .into(),
                                )
                                .build(
                                    self,
                                    Definition {
                                        name: name.clone(),
                                        kind: DefKind::FunctionSym,
                                    },
                                );
                            defs.push(def);
                        }
                        let new_scope = self.make_scope(
                            names,
                            ScopeKind::ModuleItem(ModuleItem::DefinitionId(*defs.last()?)),
                        );
                        self.lower_sort_opt(declare.sort());

                        self.push_scope(new_scope);

                        Some(defs.into_iter().map(Into::into).collect())
                    }
                    ast::ConstantDeclareDir::PrefixConstantDeclare(_) => {
                        // todo!(); TODO: implement
                        None
                    }
                }
            }
            ast::Dir::DeclareDir(declare) => match declare {
                ast::DeclareDir::PrefixDeclareDir(_) => {
                    // todo!(); TODO: implement
                    None
                }
                ast::DeclareDir::InfixDeclareDir(declare) => {
                    let names: Vec<Name> = declare.names().map(|ident| ident.as_name()).collect();
                    let mut defs = Vec::new();
                    for name in &names {
                        let def = self
                            .builder::<ast::MetaDefinition>(
                                ast::FunctionSymbol::from(ast::DeclareDir::from(declare.clone()))
                                    .into(),
                            )
                            .build(
                                self,
                                Definition {
                                    name: name.clone(),
                                    kind: DefKind::FunctionSym,
                                },
                            );
                        defs.push(def);
                    }
                    let new_scope = self.make_scope(
                        names,
                        ScopeKind::ModuleItem(ModuleItem::DefinitionId(*defs.last()?)),
                    );

                    let sort_var_scope = if let Some(sort_vars) = declare.sort_vars_decl() {
                        let mut vars = Vec::new();
                        for sv in sort_vars.ident_sort_decls() {
                            vars.push(or_continue!(sv.name()).as_name());
                        }
                        let def = self
                            .builder::<ast::MetaDefinition>(
                                ast::FunctionSymbol::from(ast::DeclareDir::from(declare.clone()))
                                    .into(),
                            )
                            .build(
                                self,
                                Definition {
                                    name: vars[0].clone(),
                                    kind: DefKind::Sort,
                                },
                            );
                        Some(self.make_scope(vars, ScopeKind::ModuleItem(def.into())))
                    } else {
                        None
                    };

                    let pop_scope = sort_var_scope.is_some();
                    if let Some(scope) = sort_var_scope {
                        self.push_scope(scope);
                    }

                    if let Some(sorts) = declare.func_sorts() {
                        for sort in sorts.sorts() {
                            self.lower_sort(sort);
                        }
                    }

                    self.lower_sort_opt(declare.return_sort());

                    if pop_scope {
                        self.pop_scope();
                    }

                    self.push_scope(new_scope);

                    Some(defs.into_iter().map(Into::into).collect())
                }
            },
            ast::Dir::DefineDir(define) => match define {
                ast::DefineDir::InfixDefineDir(define) => self.lower_define(define),
                ast::DefineDir::PrefixDefineDir(define) => match define {
                    ast::PrefixDefineDir::PrefixDefine(define) => self.lower_define(define),
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

    fn lower_sort_opt(&mut self, sort: Option<ast::Sort>) -> Option<SortId> {
        sort.and_then(|sort| self.lower_sort(sort))
    }

    #[tracing::instrument(skip(self, define))]
    fn lower_define<Def>(&mut self, define: Def) -> Option<Vec<ModuleItem>>
    where
        Def: AstNode + HasDefineName + HasDefineBody + Clone,
        ast::Definition: From<Def>,
    {
        let define_name = define.define_name()?;
        let names = match define_name {
            ast::DefineName::Name(id) => vec![id.as_name()],
            ast::DefineName::DefineNamedPattern(named) => {
                vec![named.name()?.as_name()]
            }
            ast::DefineName::DefineProc(proc) => {
                // let args =
                let name = proc.name()?.as_name();
                let def = self.alloc_definition(Definition {
                    name: name.clone(),
                    kind: DefKind::Value,
                });

                let outer_scope = self.make_scope(vec![name], ScopeKind::ModuleItem(def.into()));
                let src = self.make_source(&ast::Definition::from(define.clone()).into());
                self.set_definition_source(src, def);
                self.set_module_item_scope(ModuleItem::DefinitionId(def), outer_scope);

                let args = self.lower_args(proc.args());

                let inner_scope = self.make_scope(args, ScopeKind::ModuleItem(def.into()));

                self.with_scope(inner_scope, |ctx| {
                    let _body = ctx.lower_phrase(define.define_body()?);

                    Some(_body)
                });

                self.push_scope(outer_scope);

                return Some(vec![def.into()]);
            }
            ast::DefineName::ListPat(list_pat) => {
                let lowered = self.lower_pat(list_pat.into())?;

                if let Some(scope) = lowered.1 {
                    self.push_scope(scope);
                }

                self.lower_phrase(define.define_body()?);
                return Some(vec![]);
                // todo!(); TODO: implement
                // Vec::new()
            }
        };

        let src = self.make_source(&ast::Definition::from(define.clone()).into());

        let mut module_items: Vec<ModuleItem> = Vec::new();
        for name in names.clone() {
            let def = self.alloc_definition(Definition {
                name,
                kind: DefKind::Value,
            });
            self.set_definition_source(src.clone(), def);
            module_items.push(def.into());
        }

        let new_scope = self.make_module_item_scope(names, *module_items.last()?);

        self.push_scope(new_scope);

        self.lower_phrase(define.define_body()?);

        Some(module_items)
    }

    fn lower_pat(&mut self, pat: ast::Pat) -> Option<(PatId, Option<ScopeId>)> {
        match pat.clone() {
            ast::Pat::IdentPat(ident_pat) => {
                let name = self.lower_param(ident_pat.maybe_wildcard_typed_param()?)?;
                let (pat, scope) = self
                    .builder(pat)
                    .introduces(Some(name.clone()))
                    .build_binding(
                        self,
                        Pat {
                            kind: PatKind::Ident(name),
                        },
                    );
                Some((pat, Some(scope)))
            }
            ast::Pat::ListPat(list_pat) => {
                let mut pats = Vec::new();
                let mut scope = None;
                for pat in list_pat.pats() {
                    if let Some(temp_scope) = scope {
                        if let Some((p, s)) =
                            self.with_scope(temp_scope, |ctx| ctx.lower_pat(pat.clone()))
                        {
                            pats.push(p);
                            if let Some(s) = s {
                                scope = Some(s);
                            }
                        }
                    } else if let Some((pat, s)) = self.lower_pat(pat) {
                        pats.push(pat);
                        if let Some(s) = s {
                            scope = Some(s);
                        }
                    }
                }
                let scope = scope.unwrap_or_else(|| self.current_scope());
                let (pat, scope) = self.builder(pat).with_scope(scope).build_binding(
                    self,
                    Pat {
                        kind: PatKind::List(pats),
                    },
                );
                self.add_pat_bindings(scope, pat);
                Some((pat, Some(scope)))
            }
            ast::Pat::VarPat(_)
            | ast::Pat::MetaIdentPat(_)
            | ast::Pat::LiteralPat(_)
            | ast::Pat::UnitPat(_)
            | ast::Pat::NamedPat(_)
            | ast::Pat::ValOfPat(_)
            | ast::Pat::ListOfPat(_)
            | ast::Pat::SplitPat(_)
            | ast::Pat::CompoundPat(_)
            | ast::Pat::WherePat(_)
            | ast::Pat::SomeThingPat(_) => {
                // todo!(); TODO: implement
                None
            }
        }
    }

    fn add_pat_bindings(&mut self, scope: ScopeId, pat: PatId) {
        let pattern = self.hir[pat].clone();
        if let PatKind::Ident(ref name) = pattern.kind {
            self.scopes[scope].introduced.push(name.clone());
        }
        pattern.walk_child_pats(|sub| self.add_pat_bindings(scope, sub));
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> Option<ExprId> {
        match &expr {
            ast::Expr::IdentExpr(_)
            | ast::Expr::LiteralExpr(_)
            | ast::Expr::UnitExpr(_)
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
                let id = self.builder(expr.clone()).build(self, Expr {});

                walk_child_exprs(expr, |exp| {
                    self.lower_expr(exp);
                });

                Some(id)
            }
            ast::Expr::TermVarExpr(var) => {
                let _sort = var.sort().and_then(|s| self.lower_sort(s));
                let id = self.builder(expr.clone()).build(self, Expr {});

                Some(id)
            }
            ast::Expr::LambdaExpr(lambda) => {
                let arg_names = self.lower_args(lambda.maybe_wildcard_typed_params());

                let lambda_expr = self.builder(expr.clone()).build(self, Expr {});

                let body_scope = self.make_scope(arg_names, ScopeKind::Expr(lambda_expr));

                let body = self.with_scope(body_scope, |ctx| ctx.lower_expr(lambda.expr()?))?;

                let _ = body; //TODO: model lambda and set the body here

                Some(lambda_expr)
            }
            ast::Expr::MethodExpr(method) => {
                let arg_names = self.lower_args(method.maybe_wildcard_typed_params());

                let method_expr = self.builder(expr.clone()).build(self, Expr {});

                let body_scope = self.make_scope(arg_names, ScopeKind::Expr(method_expr));

                let body = self.with_scope(body_scope, |ctx| ctx.lower_ded(method.ded()?))?;

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

    fn lower_phrase(&mut self, phrase: ast::Phrase) -> Option<PhraseId> {
        match phrase {
            ast::Phrase::ExprPhrase(exp) => {
                let id = self.lower_expr(exp.expr()?)?;
                Some(id.into())
            }
            ast::Phrase::DedPhrase(d) => {
                let id = self.lower_ded(d.ded()?)?;
                Some(id.into())
            }
        }
    }

    fn lower_ded(&mut self, ded: ast::Ded) -> Option<DedId> {
        walk_child_exprs(ded.clone(), |expr| {
            self.lower_expr(expr);
        });

        let id = self.builder(ded).build(self, Ded {});

        Some(id)
    }

    fn lower_sort_decl(
        &mut self,
        sort_decl: &ast::SortDecl,
        src: DefinitionSource,
    ) -> Option<DefinitionId> {
        let name = sort_decl_name(sort_decl)?;

        let domain_id = self.alloc_definition(Definition {
            name: name.clone(),
            kind: DefKind::Sort,
        });

        let new_scope = self.make_scope(vec![name], ScopeKind::ModuleItem(domain_id.into())); // FIXME: this is wrong, there should be a shared scope for all the introduced sorts
        self.set_module_item_scope(domain_id.into(), new_scope);
        self.set_definition_source_bw(src, domain_id);

        self.push_scope(new_scope);

        // let _sort_id = self.lower_sort(sort_decl.clone().into())?;

        Some(domain_id)
    }

    fn lower_sort(&mut self, sort: ast::Sort) -> Option<SortId> {
        let kind = match sort.clone() {
            ast::Sort::VarSort(v) => {
                let v = ast::Ident::cast(v.ident_token()?).unwrap().as_name();
                SortKind::Var(v)
            }
            ast::Sort::IdentSort(ident) => {
                let id = ident.name_ref()?.as_name();

                SortKind::Ident(id)
            }
            ast::Sort::CompoundSort(compound) => {
                let mut sorts = Vec::new();
                for s in compound.sorts() {
                    if let Some(sort_id) = self.lower_sort(s) {
                        sorts.push(sort_id);
                    }
                }
                SortKind::Compound(sorts)
            }
        };

        let id = self
            .builder::<ast::SortLike>(sort.into())
            .build(self, Sort { kind });

        Some(id)
    }

    fn lower_param(&mut self, param: MaybeWildcardTypedParam) -> Option<Name> {
        Some(match param {
            ast::MaybeWildcardTypedParam::MaybeTypedParam(p) => match p {
                ast::MaybeTypedParam::Name(id) => id.as_name(),
                ast::MaybeTypedParam::TypedParam(tp) => {
                    if let Some(sort) = tp.sort() {
                        self.lower_sort(sort);
                    }
                    tp.name()?.as_name()
                }
                ast::MaybeTypedParam::OpAnnotatedParam(op) => op.name()?.as_name(),
            },
            ast::MaybeWildcardTypedParam::Wildcard(_) => return None,
        })
    }

    fn lower_args(&mut self, args: AstChildren<MaybeWildcardTypedParam>) -> Vec<Name> {
        let mut arg_names = Vec::new();
        for p in args {
            let arg = or_continue!(self.lower_param(p));
            arg_names.push(arg);
        }
        arg_names
    }
}

fn sort_decl_name(sort_decl: &ast::SortDecl) -> Option<Name> {
    match sort_decl {
        ast::SortDecl::IdentSortDecl(ident) => Some(ident.name()?.as_name()),
        ast::SortDecl::CompoundSortDecl(compound) => compound
            .ident_sort_decls()
            .next()?
            .name()
            .map(|n| n.as_name()),
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

trait WithSource {
    type Source: Clone;
    fn set_source(&self, source: Self::Source, source_map: &mut FileHirSourceMap);
}

#[duplicate::duplicate_item(
    id_ty               source_ty               hir_map             ;
    [DefinitionId]      [DefinitionSource]      [definitions]       ;
    [SortId]            [SortSource]            [sorts]             ;
    [DedId]             [DedSource]             [deds]              ;
    [ExprId]            [ExprSource]            [exprs]             ;
    [ModuleId]          [ModuleSource]          [modules]           ;
    [NameRefId]      [NameRefSource]      [name_refs]       ;
    [PatId]             [PatSource]             [pats]              ;
)]
impl WithSource for id_ty {
    type Source = source_ty;
    fn set_source(&self, source: Self::Source, source_map: &mut FileHirSourceMap) {
        source_map.hir_map.insert(source.clone(), *self);
        paste! {
            source_map.[<hir_map _back>].insert(*self, source);
        }
    }
}

trait WithScope: Sized {
    type ScopeMapKey: From<Self> + Clone;
    fn set_scope(&self, scope: ScopeId, scopes: &mut ScopeTree);
}

#[duplicate::duplicate_item(
    id_ty               scope_map_key       set_scope_fn ;
    [DefinitionId]      [ModuleItem]        [set_module_item_scope] ;
    [ModuleId]          [ModuleItem]        [set_module_item_scope] ;
    [SortId]            [Self]              [set_sort_scope]        ;
    [DedId]             [Self]              [set_ded_scope]         ;
    [ExprId]            [Self]              [set_expr_scope]        ;
    [NameRefId]      [Self]              [set_identifier_scope]  ;
    [PatId]             [Self]              [set_pat_scope]         ;
)]
impl WithScope for id_ty {
    type ScopeMapKey = scope_map_key;
    fn set_scope(&self, scope: ScopeId, scopes: &mut ScopeTree) {
        #[allow(clippy::useless_conversion)]
        scopes.set_scope_fn(scope_map_key::from(*self), scope);
    }
}

trait BindingItem {
    fn make_scope(
        self,
        introduces: impl IntoIterator<Item = Name>,
        parent: Option<ScopeId>,
    ) -> Scope;
}

impl<T> BindingItem for T
where
    ScopeKind: From<T>,
    T: Copy,
{
    fn make_scope(
        self,
        introduces: impl IntoIterator<Item = Name>,
        parent: Option<ScopeId>,
    ) -> Scope {
        Scope {
            parent,
            introduced: introduces.into_iter().collect(),
            kind: ScopeKind::from(self),
        }
    }
}

trait MakeNode: Sized {
    fn make_node(self, hir: &mut FileHir) -> Idx<Self>;
}

#[duplicate::duplicate_item(
    node_ty                hir_map             ;
    [Definition]           [definitions]       ;
    [Sort]                 [sorts]             ;
    [Ded]                  [deds]              ;
    [Expr]                 [exprs]             ;
    [Module]               [modules]           ;
    [Pat]                  [pats]              ;
)]
impl MakeNode for node_ty {
    fn make_node(self, hir: &mut FileHir) -> Idx<node_ty> {
        hir.hir_map.alloc(self)
    }
}
