use insta::assert_snapshot;

use crate::{AstNode, Parse, SourceFile};
use std::fmt::Write;

fn display_tree(root: &Parse<SourceFile>) -> String {
    let mut buf = String::new();

    let mut indent = String::new();
    let mut depth = 0;
    let mut len = 0;
    for event in root.tree().syntax().preorder_with_tokens() {
        match event {
            rowan::WalkEvent::Enter(rowan::NodeOrToken::Token(tok)) => {
                assert!(depth > 0);
                let kind = tok.kind();
                let text = tok.text();
                len += text.len();
                writeln!(buf, "{indent}{kind:?} {text:?}").unwrap();
            }
            rowan::WalkEvent::Enter(rowan::NodeOrToken::Node(node)) => {
                assert!(depth > 0 || len == 0);
                depth += 1;
                let kind = node.kind();
                writeln!(buf, "{indent}{kind:?}").unwrap();
                indent.push_str("  ");
            }
            rowan::WalkEvent::Leave(rowan::NodeOrToken::Token(_)) => {}
            rowan::WalkEvent::Leave(rowan::NodeOrToken::Node(_)) => {
                assert!(depth > 0);
                depth -= 1;
                indent.pop();
                indent.pop();
            }
        }
    }

    for error in root.errors() {
        let e = root.syntax_node().covering_element(error.range());
        writeln!(buf, "error {:?}: {}", e, error).unwrap();
    }

    buf
}

#[test]
fn syntax_validation() {
    insta::glob!("../test_data", "validation/*.ath", |path| {
        let contents = std::fs::read_to_string(path).unwrap();
        eprintln!("running test: {:?}", path.file_name().unwrap());
        let tree = crate::ast::SourceFile::parse(&contents);
        let actual = display_tree(&tree);
        assert!(tree.errors().len() > 0, "no errors:\n{actual}",);
        insta::with_settings!({description => &contents, omit_expression => true }, { assert_snapshot!(actual) });
    });
}
