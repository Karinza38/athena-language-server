use std::fmt::Debug;

use hir::{
    file_hir::ExprSource,
    name::{AsName, Name},
    scope::{Scope, ScopeKind},
    FileSema, HasHir, HasSyntaxNodePtr, HirDatabase, HirNode, InFile,
};
use ide_db::{
    base_db::{FilePosition, SourceDatabase},
    RootDatabase,
};
use syntax::{
    ast, match_ast, AstNode, AstPtr, AstToken, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken,
    TextRange,
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

    let Some(def) = find_def_new(
        db,
        InFile {
            file_id: position.file_id,
            value: token.clone(),
        },
    ) else {
        tracing::info!("no definition found");
        return None;
    };

    Some(RangeInfo {
        range: token.text_range(),
        info: vec![def],
    })
}

#[derive(Debug, Clone)]
enum Interesting {
    Expr(ast::Expr),
    Sort(ast::Sort),
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
                ast::Sort(it) => {
                    return Some(Interesting::Sort(it));
                },
                _ => {}
            }
        }
        node = parent.parent();
    }
    None
}

fn find_focus_range(name: &Name, full_node: &SyntaxNode) -> Option<TextRange> {
    for desc in full_node.descendants_with_tokens() {
        match desc {
            NodeOrToken::Token(tok) => {
                if let Some(ident) = ast::Ident::cast(tok.clone()) {
                    if ident.as_name() == *name {
                        return Some(ident.syntax().text_range());
                    }
                }
            }
            NodeOrToken::Node(_) => {}
        }
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

    let root = db.parse(file_id).syntax_node();

    let scope = match node.clone() {
        Interesting::Expr(expr) => to_scope(expr, &sema),
        Interesting::Sort(sort) => to_scope(sort, &sema),
    }?;

    let scope = find_defining_scope(&name, &scope, &sema)?;

    tracing::info!("found defining scope: {:?}", scope.kind);
    let node = scope_kind_to_node(scope.kind, &root, &sema)?;
    Some(NavigationTarget {
        full_range: node.text_range(),
        focus_range: find_focus_range(&name, &node),
        file_id,
        name: name.as_smol_str(),
    })
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

#[tracing::instrument(skip(sema))]
fn to_scope<N: AstNode + Debug>(node: N, sema: &FileSema) -> Option<&Scope>
where
    InFile<AstPtr<N>>: HasHir,
{
    let source = InFile::new(sema.file_id, AstPtr::new(&node));
    Some(sema.scope(sema.get_scope_for_source(source)?))
}

#[tracing::instrument(skip(root, sema))]
fn scope_kind_to_node(scope: ScopeKind, root: &SyntaxNode, sema: &FileSema) -> Option<SyntaxNode> {
    match scope {
        ScopeKind::Expr(e) => {
            let source: ExprSource = sema.get_source_for::<hir::expr::Expr>(e)?;
            Some(source.value.syntax_node_ptr().to_node(root))
        }
        ScopeKind::ModuleItem(mi) => match mi {
            hir::file_hir::ModuleItem::ModuleId(m) => {
                id_to_node::<hir::file_hir::Module>(m, root, sema)
            }
            hir::file_hir::ModuleItem::DefinitionId(id) => {
                id_to_node::<hir::file_hir::Definition>(id, root, sema)
            }
            hir::file_hir::ModuleItem::PhraseId(_) => {
                // id_to_node::<hir::phrase::Phrase>(id, root, sema)
                todo!()
            }
            hir::file_hir::ModuleItem::StructureId(_) => todo!(),
            hir::file_hir::ModuleItem::DataTypeId(_) => todo!(),
        },
        ScopeKind::Root => None,
    }
}

#[tracing::instrument(skip(root, sema))]
fn id_to_node<N: HirNode>(id: N::Id, root: &SyntaxNode, sema: &FileSema) -> Option<SyntaxNode>
where
    N::Id: Debug,
{
    let source: N::Source = sema.get_source_for::<N>(id)?;
    Some(source.syntax_node_ptr().to_node(root))
}

#[cfg(test)]
mod tests {
    use crate::{fixture, FileRange};
    use itertools::Itertools;

    #[track_caller]
    fn check(ra_fixture: &str) {
        let (analysis, position, expected) = fixture::annotations(ra_fixture);
        let navs = analysis
            .go_to_definition(position)
            .unwrap()
            .expect("no definition found")
            .info;

        let cmp = |&FileRange { file_id, range }: &_| (file_id, range.start());
        let navs = navs
            .into_iter()
            .map(|nav| FileRange {
                file_id: nav.file_id,
                range: nav.focus_or_full_range(),
            })
            .sorted_by_key(cmp)
            .collect::<Vec<_>>();
        let expected = expected
            .into_iter()
            .map(|(FileRange { file_id, range }, _)| FileRange { file_id, range })
            .sorted_by_key(cmp)
            .collect::<Vec<_>>();
        assert_eq!(expected, navs);
    }

    #[allow(dead_code)]
    fn check_unresolved(ra_fixture: &str) {
        let (analysis, position) = fixture::position(ra_fixture);
        let navs = analysis
            .go_to_definition(position)
            .unwrap()
            .expect("no definition found")
            .info;

        assert!(
            navs.is_empty(),
            "didn't expect this to resolve anywhere: {navs:?}"
        )
    }

    #[test]
    fn domain_def() {
        check(
            r#"
            module Foo {
                domain Bar
                //     ^^^
                declare fun : [] -> $0Bar
            }
            "#,
        )
    }

    #[test]
    fn domains_def() {
        check(
            r#"
            module Foo {
                domains A, B, C
                //         ^
                declare fun : [] -> $0B
            }
            "#,
        )
    }
}
