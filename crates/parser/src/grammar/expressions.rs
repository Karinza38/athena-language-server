use super::{identifier, literal};
use crate::{
    parser::Parser,
    SyntaxKind::{CHAR, IDENT, IDENT_EXPR, STRING},
    T,
};

// test(expr) simple_ident_expr
// foo
pub(crate) fn ident_expr(p: &mut Parser) {
    assert!(p.at(IDENT));
    let m = p.start();
    identifier(p);
    m.complete(p, IDENT_EXPR);
}

pub(crate) fn expr(p: &mut Parser) {
    let m = p.start();
    if p.at(IDENT) {
        ident_expr(p);
    } else if p.at_one_of(super::LIT_SET) {
        literal(p);
    } else {
        todo!();
    }
}
