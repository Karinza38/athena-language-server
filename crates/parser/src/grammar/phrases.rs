use crate::{parser::Parser, SyntaxKind};

fn expr_phrase(p: &mut Parser) -> bool {
    let m = p.start();
    if super::expressions::expr(p) {
        m.complete(p, SyntaxKind::EXPR_PHRASE);
        true
    } else {
        m.abandon(p);
        false
    }
}

pub(crate) fn phrase(p: &mut Parser) -> bool {
    // FIXME: Do proper lookahead instead of just backtracking
    if expr_phrase(p) {
        true
    } else {
        // TODO: Add other phrase types
        false
    }
}
