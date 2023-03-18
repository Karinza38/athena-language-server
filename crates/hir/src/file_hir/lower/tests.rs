use base_db::{fixture::WithFixture, FilePosition, SourceDatabase};
use syntax::{ast, AstNode, AstPtr, SyntaxKind, SyntaxNode, SyntaxToken, TextSize, TokenAtOffset};

use crate::{db::HirDatabase, test_db::TestDB, FileSema, InFile};

fn lower(fixture: &str) -> (TestDB, FilePosition, FileSema) {
    let (db, file_pos) = TestDB::with_position(fixture);

    let sema = db.file_sema(file_pos.file_id);
    (db, file_pos, sema)
}

/// Picks the token with the highest rank returned by the passed in function.
pub fn pick_best_token(
    tokens: TokenAtOffset<SyntaxToken>,
    f: impl Fn(SyntaxKind) -> usize,
) -> Option<SyntaxToken> {
    tokens.max_by_key(move |t| f(t.kind()))
}

pub fn pick_token(node: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    pick_best_token(node.token_at_offset(offset), |kind| match kind {
        SyntaxKind::IDENT => 4,
        k if k.is_trivia() => 0,
        _ => 1,
    })
}

fn find_node<N: AstNode>(token: SyntaxToken) -> Option<N> {
    let mut node = token.parent();
    while let Some(parent) = node {
        if let Some(res) = N::cast(parent.clone()) {
            return Some(res);
        }
        node = parent.parent();
    }
    None
}

#[test]
fn lower_it() {
    let (db, src, hir) = lower(
        r#"
		module A { domains $0 B, C }
		"#,
    );

    let tree = db.parse(src.file_id).tree();

    let tok = pick_token(&tree.syntax(), src.offset).unwrap();

    let parent = find_node::<ast::MetaDefinition>(tok).unwrap();

    let parent_source = InFile::new(src.file_id, AstPtr::new(&parent));
    let hid = hir
        .file_hir_source_map
        .definitions
        .get(&parent_source)
        .unwrap();

    let h = &hir.file_hir.definitions[*hid];

    eprintln!("{:#?}", h);
    // let tree = eprintln!("{hir:#?}");
    // panic!();
}

// #[test]
// fn lower_nested_lambda() {
//     let (db, src, hir) = lower(
//         r#"
// 		define foo := lambda (a) (lambda (b) ($0 a b))
// 		"#,
//     );

//     let tree = db.parse(src.file_id).tree();

//     let tok = pick_token(&tree.syntax(), src.offset).unwrap();

//     let parent = find_node::<ast::MetaDefinition>(tok).unwrap();

//     let parent_source = InFile::new(src.file_id, AstPtr::new(&parent));
//     let hid = hir
//         .file_hir_source_map
//         .definitions
//         .get(&parent_source)
//         .unwrap();

//     let h = &hir.file_hir.definitions[*hid];

//     eprintln!("{:#?}", hir);
//     // let tree = eprintln!("{hir:#?}");
//     panic!();
// }
