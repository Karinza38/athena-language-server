use crate::{ast, match_ast, AstNode, SyntaxError, SyntaxNode};

pub(crate) fn validate(root: &SyntaxNode) -> Vec<SyntaxError> {
    let mut errors = Vec::new();
    for node in root.descendants() {
        match_ast! {
            match node {
                ast::ByDed(it) => validate_by_ded_starts_with_expr(it, &mut errors),
                _ => {}
            }
        }
    }
    errors
}

fn validate_by_ded_starts_with_expr(by_ded: ast::ByDed, errors: &mut Vec<SyntaxError>) {
    match by_ded.phrase() {
        Some(ast::Phrase::Expr(_)) => {}
        Some(phrase) => {
            let error = SyntaxError::new(
                "By deduction must start with an expression",
                phrase.syntax().text_range(),
            );
            errors.push(error);
        }
        _ => {
            unreachable!("Should have failed to parse");
        }
    }
}
