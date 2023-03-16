use hir::InFile;
use ide_db::{
    base_db::{FilePosition, SourceDatabase},
    RootDatabase,
};
use syntax::{ast, match_ast, AstNode, AstToken, SyntaxKind, SyntaxNode, SyntaxToken, NodeOrToken};

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

    let def = find_def(
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
