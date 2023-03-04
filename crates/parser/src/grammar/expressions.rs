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

// test(expr) simple_literal_expr
// "fooby"
pub(crate) fn literal_expr(p: &mut Parser) {
    assert!(p.at_one_of(super::LIT_SET));
    let m = p.start();
    literal(p);
    m.complete(p, IDENT_EXPR);
}

pub(crate) fn expr(p: &mut Parser) {
    if p.at(IDENT) {
        ident_expr(p);
    } else if p.at_one_of(super::LIT_SET) {
        literal_expr(p);
    } else {
        todo!();
    }
}
