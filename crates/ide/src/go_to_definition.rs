use hir::{name::AsName, scope::ScopeKind, HirDatabase, InFile};
use ide_db::{
    base_db::{FilePosition, SourceDatabase},
    RootDatabase,
};
use syntax::{
    ast::{self, HasDefineName},
    match_ast, AstNode, AstPtr, AstToken, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken,
};

use crate::{helpers::pick_best_token, navigation_target::NavigationTarget, RangeInfo};

#[tracing::instrument]
pub(crate) fn go_to_definition(
    db: &RootDatabase,
    position: FilePosition,
) -> Option<RangeInfo<Vec<NavigationTarget>>> {
    let file = db.parse(position.file_id).syntax_node();
    let token = pick_best_token(file.token_at_offset(position.offset), |kind| match kind {
        SyntaxKind::IDENT => 4,
        k if k.is_trivia() => 0,
        _ => 1,
    })?;

    tracing::info!(?token, "at offset");

    let def = find_def_new(
        db,
        InFile {
            file_id: position.file_id,
            value: token.clone(),
        },
    )?;

    Some(RangeInfo {
        range: token.text_range(),
        info: vec![def],
    })
}

enum Interesting {
    MetaDefinition(ast::MetaDefinition),
    Expr(ast::Expr),
    Module(ast::Module),
}

#[tracing::instrument]
fn find_interesting_node(tok: &SyntaxToken) -> Option<Interesting> {
    let mut node = tok.parent();
    while let Some(parent) = node {
        match_ast! {
            match parent {
                ast::Expr(it) => {
                    return Some(Interesting::Expr(it));
                },
                _ => {}
            }
        }
        node = parent.parent();
    }
    None
}

#[tracing::instrument]
fn find_def_new(
    db: &RootDatabase,
    InFile {
        file_id,
        value: token,
    }: InFile<SyntaxToken>,
) -> Option<NavigationTarget> {
    let Some(ident) = ast::Ident::cast(token.clone()) else {
        return None;
    };
    let name = ident.as_name();

    tracing::info!(?name, "finding interesting node");
    let node = find_interesting_node(&token)?;

    let sema = db.file_sema(file_id);

    match node {
        Interesting::MetaDefinition(_) => todo!(),
        Interesting::Expr(expr) => {
            tracing::info!("it's an expr: {:?}", expr);
            let source = InFile::new(file_id, AstPtr::new(&expr));

            tracing::info!("looking up expr id");
            let expr_id = *sema.file_hir_source_map.exprs.get(&source)?;
            tracing::info!("looking up scope id");
            let scope_id = sema.scope_tree.scope_by_expr(expr_id)?;
            tracing::info!("looking up scope");
            let mut scope = sema.scope_tree.scope(scope_id);

            tracing::info!(?scope, "finding defining scope");
            while scope.kind != ScopeKind::Root {
                tracing::info!(?scope, "considering scope");
                if scope.introduced.contains(&name) {
                    tracing::info!(?scope, "found defining scope");
                    let range = match scope.kind {
                        ScopeKind::Expr(_) => todo!(),
                        ScopeKind::ModuleItem(item) => match item {
                            hir::file_hir::ModuleItem::ModuleId(_) => todo!(),
                            hir::file_hir::ModuleItem::DefinitionId(id) => {
                                let node =
                                    sema.file_hir_source_map.definitions_back.get(id).unwrap();
                                node.value.text_range()
                            }
                            hir::file_hir::ModuleItem::DataTypeId(_) => todo!(),
                            hir::file_hir::ModuleItem::StructureId(_) => todo!(),
                            hir::file_hir::ModuleItem::PhraseId(_) => todo!(),
                        },
                        ScopeKind::Root => todo!(),
                    };
                    return Some(NavigationTarget {
                        full_range: range,
                        focus_range: None,
                        file_id,
                        name: name.as_smol_str(),
                    });
                }
                scope = sema.scope_tree.scope(scope.parent?);
            }
        }
        Interesting::Module(_) => todo!(),
    }
    tracing::info!("no definition found");
    None
}

#[tracing::instrument]
fn find_def(
    db: &RootDatabase,
    InFile {
        file_id,
        value: token,
    }: InFile<SyntaxToken>,
) -> Option<NavigationTarget> {
    let ident = ast::Ident::cast(token)?;
    let name = ident.text();

    tracing::debug!(?name, "ident has name");

    let stmt = find_stmt(ident.syntax())?;
    for sibling in stmt.siblings_with_tokens(syntax::Direction::Prev) {
        let NodeOrToken::Node(sibling) = sibling else {
            continue;
        };
        tracing::debug!(?sibling, "sibling");
        match_ast! {
            match sibling {
                ast::DirStmt(it) => {
                    if let Some(def) = ast::Definition::cast(it.syntax().clone()) {
                        match def {
                            ast::Definition::InfixDefineDir(it) => {
                                if let Some(def) = find_def_in_define_name(db, &name, InFile { file_id, value: it.define_name()? }) {
                                    return Some(def);
                                }
                            }
                            ast::Definition::PrefixDefineDir(it) => {
                                let ast::PrefixDefineDir::PrefixDefine(define) = it else {
                                    continue
                                };
                                if let Some(def) = find_def_in_define_name(db, &name, InFile { file_id, value: define.define_name()? }) {
                                    return Some(def);
                                }
                            }
                        }
                    }
                },
                _ => {

                }
            }
        }
    }

    None
}

fn find_stmt(tok: &SyntaxToken) -> Option<SyntaxNode> {
    for ancestor in tok.parent_ancestors() {
        match_ast! {
            match ancestor {
                ast::Stmt(it) => {
                    return Some(it.syntax().clone());
                },
                _ => {

                }
            }
        }
    }
    None
}

fn find_def_in_define_name(
    db: &RootDatabase,
    want: &str,
    InFile {
        file_id,
        value: name,
    }: InFile<ast::DefineName>,
) -> Option<NavigationTarget> {
    match name.clone() {
        ast::DefineName::Identifier(id) => {
            let ident_tok = id.ident_token()?;
            let candidate = ident_tok.text();
            tracing::debug!(?id, ?candidate, "inspecting identifier");
            if candidate == want {
                return Some(NavigationTarget {
                    file_id,
                    full_range: name.syntax().parent()?.text_range(),
                    focus_range: Some(id.syntax().text_range()),
                    name: want.into(),
                });
            }
        }
        _ => {}
    }

    None
}
