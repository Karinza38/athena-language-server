use std::fmt::Debug;

use hir::{
    file_hir::{ExprSource, PatSource},
    name::{AsName, Name},
    semantics::NameRefResolution,
    FileSema, HasSyntaxNodePtr, HirNode, InFile, Semantics,
};
use ide_db::{
    base_db::{FilePosition, SourceDatabase},
    RootDatabase,
};
use syntax::{ast, AstNode, AstToken, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};

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
        return Some(RangeInfo::new(token.text_range(), Vec::new()));
    };

    Some(RangeInfo {
        range: token.text_range(),
        info: vec![def],
    })
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

    let name_ref = ast::NameRef::cast(token.parent()?)?;

    let sema = Semantics::new(db);

    let nr = InFile::new(file_id, name_ref);
    let res = sema.resolve_name_ref(nr)?;

    let root = db.parse(file_id).syntax_node();

    let node = name_res_to_node(res, &root, &sema.file_sema(file_id))?;

    Some(NavigationTarget {
        full_range: node.text_range(),
        focus_range: find_focus_range(&name, &node),
        file_id,
        name: name.as_smol_str(),
    })
}

#[tracing::instrument(skip(root, sema))]
fn name_res_to_node(
    scope: NameRefResolution,
    root: &SyntaxNode,
    sema: &FileSema,
) -> Option<SyntaxNode> {
    match scope {
        NameRefResolution::Local(hir::semantics::Local::Expr(e)) => {
            let source: ExprSource = sema.get_source_for::<hir::expr::Expr>(e)?;
            Some(source.value.syntax_node_ptr().to_node(root))
        }
        NameRefResolution::ModuleItem(mi) => match mi {
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
        NameRefResolution::Local(hir::semantics::Local::Pat(p)) => {
            let source: PatSource = sema.get_source_for::<hir::pat::Pat>(p)?;
            Some(source.value.syntax_node_ptr().to_node(root))
        }
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
        // calling the parameter `ra_fixture` seems to make rust analyzer highlight the fixture more nicely (presumably because rust-analyzer uses fixtures internally)
        test_utils::init_logging();
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

    #[test]
    fn function_symbol_def() {
        check(
            r#"
            module Foo {
                domain Bar
                declare fun : [] -> Bar
                //      ^^^
                define a := $0fun
            }
            "#,
        )
    }

    #[test]
    fn define_list() {
        check(
            r#"
            module Foo {
                define [a [b]] := [1 [2]]
                //         ^
                define c := $0b
            }
            "#,
        )
    }

    #[test]
    fn assert() {
        check(
            r#"
            module Foo {
                assert a := (1 = 1)
                //     ^
                define b := $0a
            }
            "#,
        )
    }

    #[test]
    fn assert_closed() {
        check(
            r#"
            module Foo {
                assert* a := (1 = 1)
                //      ^
                define b := $0a
            }
            "#,
        )
    }

    #[test]
    fn term_var_is_a_constant() {
        check_unresolved(
            r#"
            module Foo {
                define x := ?$0x:A
            }
            "#,
        )
    }

    #[test]
    fn constant_declare() {
        check(
            r#"
            module Foo {
                declare x : A
                //      ^
                define y := $0x
            }
            "#,
        )
    }
}
