use super::{identifier, literal};
use crate::{
    parser::Parser,
    SyntaxKind::{self, CHAR, IDENT, IDENT_EXPR, STRING, UNIT_EXPR},
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
    m.complete(p, SyntaxKind::LITERAL_EXPR);
}

// test(expr) simple_unit_expr
// ()
pub(crate) fn unit_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));
    let m = p.start();
    super::unit(p);
    m.complete(p, UNIT_EXPR);
}

// test(expr) simple_term_var_expr
// ?foo:bar
pub(crate) fn term_var_expr(p: &mut Parser) {
    assert!(p.at(T![?]));
    let m = p.start();
    p.bump(T![?]);

    if p.at(IDENT) {
        identifier(p);
    } else {
        p.error("Expected to find an identifier for the term variable");
    }

    p.expect(T![:]);

    if super::sorts::sort(p) {
        m.complete(p, SyntaxKind::TERM_VAR_EXPR);
    } else {
        p.error("Expected to find a sort for the term variable");
        m.complete(p, SyntaxKind::TERM_VAR_EXPR);
    }
}

pub(crate) fn expr(p: &mut Parser) {
    if p.at(IDENT) {
        ident_expr(p);
    } else if p.at_one_of(super::LIT_SET) {
        literal_expr(p);
    } else if p.at(T!['(']) {
        unit_expr(p);
    } else if p.at(T![?]) {
        term_var_expr(p);
    } else {
        todo!();
    }
}
