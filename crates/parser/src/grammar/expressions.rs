use super::{identifier, literal};
use crate::{
    parser::Parser,
    SyntaxKind::{self, CHAR, IDENT, IDENT_EXPR, STRING, UNIT_EXPR},
    T,
};

// test(expr) simple_ident_expr
// foo
fn ident_expr(p: &mut Parser) {
    assert!(p.at(IDENT));
    let m = p.start();
    identifier(p);
    m.complete(p, IDENT_EXPR);
}

// test(expr) simple_literal_expr
// "fooby"
fn literal_expr(p: &mut Parser) {
    assert!(p.at_one_of(super::LIT_SET));
    let m = p.start();
    literal(p);
    m.complete(p, SyntaxKind::LITERAL_EXPR);
}

// test(expr) simple_unit_expr
// ()
fn unit_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));
    let m = p.start();
    super::unit(p);
    m.complete(p, UNIT_EXPR);
}

// test(expr) simple_term_var_expr
// ?foo:bar
fn term_var_expr(p: &mut Parser) {
    assert!(p.at(T![?]));
    let m = p.start();
    p.bump(T![?]);

    if p.at(IDENT) {
        identifier(p);
    } else {
        // test_err(expr) term_var_no_identifier
        // ? :bar
        p.error("Expected to find an identifier for the term variable");
    }

    p.expect(T![:]);

    if super::sorts::sort(p) {
        m.complete(p, SyntaxKind::TERM_VAR_EXPR);
    } else {
        // test_err(expr) term_var_no_sort
        // ?foo :
        p.error("Expected to find a sort for the term variable");
        m.complete(p, SyntaxKind::TERM_VAR_EXPR);
    }
}

// test(expr) simple_meta_ident_expr
// 'foo
fn meta_ident_expr(p: &mut Parser) {
    assert!(p.at(T!['\'']));
    let m = p.start();
    super::meta_ident(p);
    m.complete(p, SyntaxKind::META_IDENT_EXPR);
}

// test(expr) simple_lambda_expr
// lambda (x y z) "hello world"
fn lambda_expr(p: &mut Parser) {
    assert!(p.at(T![lambda]));

    let m = p.start();
    p.bump(T![lambda]);
    p.expect(T!['(']);

    // test(expr) lambda_expr_no_args
    // lambda () "hello world"
    while p.at(IDENT) {
        identifier(p);
    }
    p.expect(T![')']);

    if !expr(p) {
        // test_err(expr) simple_lambda_expr_error
        // lambda (x y z) domain D
        p.error("Expected to find an expression for the lambda body");
    }
    m.complete(p, SyntaxKind::LAMBDA_EXPR);
}

// test(expr) simple_application_expr
// (foo bar baz)
fn application_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    expr(p);

    // test(expr) application_expr_no_args
    // (foo)
    while super::phrases::phrase(p) {
        // test(expr) nested_application_expr
        // (foo (bar baz))
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::APPLICATION_EXPR);
}

// test(expr) simple_list_expr
// [foo bar "baz"]
fn list_expr(p: &mut Parser) {
    assert!(p.at(T!['[']));

    let m = p.start();
    p.bump(T!['[']);

    while super::phrases::phrase(p) {
        // test(expr) nested_list_expr
        // [foo [bar baz]]
    }

    p.expect(T![']']);
    m.complete(p, SyntaxKind::LIST_EXPR);
}

// test(expr) simple_and_expr
// (&& foo bar true)
fn and_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    p.bump(T![&&]);

    while super::phrases::phrase(p) {
        // test(expr) nested_and_expr
        // (&& bar (&& baz))
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::AND_EXPR);
}

// test(expr) simple_or_expr
// (|| foo bar true)
fn or_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    p.bump(T![||]);

    while super::phrases::phrase(p) {
        // test(expr) nested_or_expr
        // (|| bar (|| baz))
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::OR_EXPR);
}

// test(expr) simple_seq_expr
// (seq foo bar baz)
fn seq_expr(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    p.bump(T![seq]);

    while super::phrases::phrase(p) {
        // test(expr) nested_seq_expr
        // (seq bar (seq baz))
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::SEQ_EXPR);
}

// test(expr) simple_string_expr
// "hello world"
pub(crate) fn expr(p: &mut Parser) -> bool {
    eprintln!("parsing expr: {:?}", p.current());

    if p.at(IDENT) {
        ident_expr(p);
    } else if p.at_one_of(super::LIT_SET) {
        literal_expr(p);
    } else if p.at(T!['(']) {
        if p.nth_at(1, T![')']) {
            unit_expr(p);
        } else if p.nth_at(1, T![&&]) {
            and_expr(p);
        } else if p.nth_at(1, T![||]) {
            or_expr(p);
        } else if p.nth_at(1, T![seq]) {
            seq_expr(p);
        } else {
            application_expr(p);
        }
    } else if p.at(T![?]) {
        term_var_expr(p);
    } else if p.at(T!['\'']) {
        meta_ident_expr(p);
    } else if p.at(T![lambda]) {
        lambda_expr(p);
    } else if p.at(T!['[']) {
        list_expr(p);
    } else {
        // todo!();
        return false;
    }
    true
}
