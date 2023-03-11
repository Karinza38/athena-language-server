use super::{
    identifier, literal,
    patterns::pat,
    phrases::{phrase, ExprOrDed, PHRASE_START_SET},
    LIT_SET,
};
use crate::{
    grammar::{deductions::ded, maybe_wildcard_typed_param},
    parser::{Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, IDENT, IDENT_EXPR, UNIT_EXPR},
    T,
};

// test(expr) simple_ident_expr
// foo

// test(expr) simple_ident_expr_with_type
// foo: Int
fn ident_expr(p: &mut Parser) {
    assert!(p.at(IDENT));
    let m = p.start();
    super::maybe_typed_param(p);
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

/// parses a unit expression with the opening '(' already consumed
fn opened_unit_expr(p: &mut Parser, m: Marker) {
    p.expect(T![')']);
    m.complete(p, UNIT_EXPR);
}

// test(expr) simple_term_var_expr
// ?foo:bar

// test(expr) term_var_no_annot
// ?foo
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

    if p.at(T![:]) {
        p.expect(T![:]);

        if !super::sorts::sort(p) {
            // test_err(expr) term_var_no_sort
            // ?foo :
            p.error("Expected to find a sort for the term variable");
        }
    }
    m.complete(p, SyntaxKind::TERM_VAR_EXPR);
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
fn lambda_expr(p: &mut Parser, is_prefix: bool) {
    assert!((is_prefix && p.at_prefix_kw(T![lambda])) || (!is_prefix && p.at(T![lambda])));

    let m = p.start();

    if is_prefix {
        p.bump(T!['(']);
    }

    p.bump(T![lambda]);
    p.expect(T!['(']);

    // test(expr) lambda_expr_no_args
    // lambda () "hello world"
    while !p.at(T![')']) && !p.at_end() {
        if !maybe_wildcard_typed_param(p) {
            // test_err(expr) lambda_expr_error_recovery
            // lambda (x y z domain D)
            p.err_recover(
                "Expected to find a parameter for the lambda",
                EXPR_START_SET,
            );
        }
    }
    p.expect(T![')']);

    if !expr(p) {
        // test_err(expr) simple_lambda_expr_error
        // lambda (x y z) domain D
        p.error("Expected to find an expression for the lambda body");
    }

    if is_prefix {
        p.expect(T![')']);
    }

    m.complete(p, SyntaxKind::LAMBDA_EXPR);
}

// test(expr) simple_application_expr
// (foo bar baz)

/// parses an application expression with the opening '(' already consumed
pub(crate) fn opened_application_expr(p: &mut Parser, m: Marker) {
    // test(expr) application_expr_no_args
    // (foo)
    while !p.at(T![')']) && !p.at_end() {
        if p.at(T![by]) {
            // FIXME: this isn't really a proper place to put this,
            // but it's the most convenient point to identify a by deduction
            super::deductions::by_ded_partial(p, m);
            return;
        } else if p.at(T![&&]) {
            // test(expr) infix_and_expr
            // (foo && bar baz)
            opened_and_expr(p, m);
            return;
        } else if p.at(T![||]) {
            // test(expr) infix_or_expr
            // (foo || bar baz)
            opened_or_expr(p, m);
            return;
        }
        // test(expr) nested_application_expr
        // (foo (bar baz))
        if !phrase(p) {
            // test_err(expr) application_expr_error
            // (foo domain D)
            p.err_recover(
                "Expected to find a phrase for the application argument",
                TokenSet::new(&[T![')']]).union(PHRASE_START_SET),
            );
        }
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

    while phrase(p) {
        // test(expr) nested_list_expr
        // [foo [bar baz]]
    }

    p.expect(T![']']);
    m.complete(p, SyntaxKind::LIST_EXPR);
}

// test(expr) simple_and_expr
// (&& foo bar true)

/// parses an and expression with the opening '(' already consumed
/// and potentially the first phrase (if this is infix)
fn opened_and_expr(p: &mut Parser, m: Marker) {
    p.bump(T![&&]);

    while !p.at(T![')']) && !p.at_end() {
        // test(expr) nested_and_expr
        // (&& bar (&& baz))
        if !phrase(p) {
            // test_err(expr) and_expr_error
            // (&& bar domain D)
            p.err_recover(
                "Expected to find a phrase in the and expression",
                TokenSet::new(&[T![')']]).union(PHRASE_START_SET),
            );
        }
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::AND_EXPR);
}

// test(expr) simple_or_expr
// (|| foo bar true)

/// parses an or expression with the opening '(' already consumed
fn opened_or_expr(p: &mut Parser, m: Marker) {
    p.bump(T![||]);

    while !p.at(T![')']) && !p.at_end() {
        // test(expr) nested_or_expr
        // (|| bar (|| baz))
        if !phrase(p) {
            // test_err(expr) or_expr_error
            // (|| bar domain D)
            p.err_recover(
                "Expected to find a phrase in the or expression",
                TokenSet::new(&[T![')']]).union(PHRASE_START_SET),
            );
        }
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::OR_EXPR);
}

// seq expression with the opening '(' already consumed

// test(expr) simple_seq_expr
// (seq foo bar baz)
fn opened_seq_expr(p: &mut Parser, m: Marker) {
    p.bump(T![seq]);

    while !p.at(T![')']) && !p.at_end() {
        // test(expr) nested_seq_expr
        // (seq bar (seq baz))
        if !phrase(p) {
            // test_err(expr) seq_expr_error
            // (seq bar domain D)
            p.err_recover(
                "Expected to find a phrase in the seq expression",
                TokenSet::new(&[T![')']]).union(PHRASE_START_SET),
            );
        }
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::SEQ_EXPR);
}

fn check_expr(p: &mut Parser) {
    super::phrases::check_expr_or_ded(p, Some(ExprOrDed::Expr));
}

// test(expr) simple_cell_expr
// cell foo
fn cell_expr(p: &mut Parser) {
    assert!(p.at(T![cell]));

    let m = p.start();
    p.bump(T![cell]);

    if !phrase(p) {
        // test_err(expr) cell_expr_no_phrase
        // cell
        p.error("Expected to find a phrase for the cell expression");
    }

    m.complete(p, SyntaxKind::CELL_EXPR);
}

// test(expr) simple_set_expr
// set! foo bar
fn set_expr(p: &mut Parser) {
    assert!(p.at(T![set!]));

    let m = p.start();
    p.bump(T![set!]);

    if !expr(p) {
        // test_err(expr) set_expr_no_expr
        // set!
        p.error("Expected to find a target expression for the set! expression");
    }

    if !phrase(p) {
        // test_err(expr) set_expr_no_phrase
        // set! foo
        p.error("Expected to find a value (phrase) for the set! expression");
    }

    m.complete(p, SyntaxKind::SET_EXPR);
}

// test(expr) simple_ref_expr
// ref foo
fn ref_expr(p: &mut Parser) {
    assert!(p.at(T![ref]));

    let m = p.start();
    p.bump(T![ref]);

    if !expr(p) {
        // test_err(expr) ref_expr_no_expr
        // ref
        p.error("Expected to find a target expression for the ref expression");
    }

    m.complete(p, SyntaxKind::REF_EXPR);
}

// test(expr) simple_make_vector_expr
// make-vector foo bar
fn make_vector_expr(p: &mut Parser) {
    assert!(p.at(T![make - vector]));

    let m = p.start();
    p.bump(T![make - vector]);

    if !expr(p) {
        // test_err(expr) make_vector_expr_no_expr
        // make-vector
        p.error("Expected to find a target expression for the make_vector expression");
    }

    if !phrase(p) {
        // test_err(expr) make_vector_expr_no_phrase
        // make-vector foo
        p.error("Expected to find a value (phrase) for the make_vector expression");
    }

    m.complete(p, SyntaxKind::MAKE_VECTOR_EXPR);
}

// test(expr) simple_vector_sub_expr
// vector-sub foo bar
fn vector_sub_expr(p: &mut Parser) {
    assert!(p.at(T![vector - sub]));

    let m = p.start();
    p.bump(T![vector - sub]);

    if !expr(p) {
        // test_err(expr) vector_sub_expr_no_expr
        // vector-sub
        p.error("Expected to find a target expression for the vector-sub expression");
    }

    if !expr(p) {
        // test_err(expr) vector_sub_expr_no_second_expr
        // vector-sub foo
        p.error("Expected to find a value (expr) for the vector-sub expression");
    }

    m.complete(p, SyntaxKind::VECTOR_SUB_EXPR);
}

// test(expr) simple_vector_set_expr
// vector-set! foo 0 bar
fn vector_set_expr(p: &mut Parser) {
    assert!(p.at(T![vector-set!]));

    let m = p.start();
    p.bump(T![vector-set!]);

    if !expr(p) {
        // test_err(expr) vector_set_expr_no_expr
        // vector-set!
        p.error("Expected to find a target vector expression for the vector-set! expression");
    }

    if !expr(p) {
        // test_err(expr) vector_set_expr_no_second_expr
        // vector-set! foo
        p.error("Expected to find an index (expr) for the vector-set! expression");
    }

    if !phrase(p) {
        // test_err(expr) vector_set_expr_no_phrase
        // vector-set! foo bar
        p.error("Expected to find a value (phrase) for the vector-set! expression");
    }

    m.complete(p, SyntaxKind::VECTOR_SET_EXPR);
}

// test(expr) simple_while_expr
// while true (print "hello world")
fn while_expr(p: &mut Parser) {
    assert!(p.at(T![while]));

    let m = p.start();
    p.bump(T![while]);

    if !phrase(p) {
        // test_err(expr) while_expr_no_expr
        // while
        p.error("Expected to find a condition (phrase) for the while expression");
    }

    if !expr(p) {
        // test_err(expr) while_expr_no_phrase
        // while foo
        p.error("Expected to find a body (expr) for the while expression");
    }

    m.complete(p, SyntaxKind::WHILE_EXPR);
}

// test(expr) simple_method
// method (x) (!claim x)
fn method_expr(p: &mut Parser) {
    assert!(p.at(T![method]));

    let m = p.start();
    p.bump(T![method]);

    p.expect(T!['(']);

    while !p.at(T![')']) && !p.at_end() {
        maybe_wildcard_typed_param(p);
    }

    p.expect(T![')']);

    if !ded(p) {
        // test_err(expr) method_expr_no_ded
        // method (foo)
        p.error("Expected to find a body (ded) for the method expression");
    }

    m.complete(p, SyntaxKind::METHOD_EXPR);
}

fn try_expr(p: &mut Parser) {
    super::phrases::try_expr_or_ded(p, Some(super::phrases::ExprOrDed::Expr));
}

fn let_expr(p: &mut Parser) {
    super::phrases::let_expr_or_ded(p, Some(super::phrases::ExprOrDed::Expr));
}

fn let_rec_expr(p: &mut Parser) {
    super::phrases::let_rec_expr_or_ded(p, Some(super::phrases::ExprOrDed::Expr));
}

// test(expr) prefix_match
// (match foo (A B) (C D))
pub(crate) fn prefix_match_clause(p: &mut Parser) {
    if !p.at(T!['(']) {
        p.error("Expected to find a prefix match clause in parens");
        return;
    }

    let m = p.start();
    p.bump(T!['(']);

    if !pat(p) {
        p.error("Expected to find a pattern for the prefix match clause");
    }

    if !expr(p) {
        // test_err(expr) prefix_match_clause_no_expr
        // (match foo ( B)
        p.error("Expected to find an expression for the prefix match clause");
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::MATCH_CLAUSE);
}

fn map_binding(p: &mut Parser) {
    let m = p.start();
    if !phrase(p) {
        // test_err(expr) map_binding_no_key
        // |{ := 1 }|
        p.error("Expected to find a key (phrase) for the map binding");
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(expr) map_binding_no_value
        // |{ foo := }|
        p.error("Expected to find a value (phrase) for the map binding");
    }
    m.complete(p, SyntaxKind::MAP_BINDING);
}

// test(expr) simple_map_expr
// |{ foo := 1, bar := 2 }|
fn map_expr(p: &mut Parser) {
    assert!(p.at(T!["|{"]));

    let m = p.start();
    p.bump(T!["|{"]);

    while !p.at(T!["}|"]) && !p.at_end() {
        p.eat(T![,]);
        map_binding(p);
    }

    p.expect(T!["}|"]);

    // test(expr) nested_map_expr
    // |{ foo := |{ bar := 1 }| }|

    m.complete(p, SyntaxKind::MAP_EXPR);
}

// test(expr) wildcard_expr
// _
fn wildcard_expr(p: &mut Parser) {
    assert!(p.at(T![_]));

    let m = p.start();
    p.bump(T![_]);
    m.complete(p, SyntaxKind::WILDCARD_EXPR);
}

/// Parses a prefix let expression.
/// the opening paren is already consumed
fn prefix_let_expr(p: &mut Parser, m: Marker) {
    assert!(p.at(T![let]) && p.peek_at(T!['(']));

    // test(expr) prefix_let_expr
    // (let ((foo 1) (bar 2)) foo)
    p.bump(T![let]);
    p.bump(T!['(']);

    while !p.at(T![')']) && !p.at_end() {
        if !super::prefix_binding(p) {
            // test_err(expr) prefix_let_expr_no_binding
            // (let (domain Foo) foo)
            p.err_recover(
                "expected a binding in the prefix let expression",
                TokenSet::new(&[T![')'], T!['(']]),
            )
        }
    }

    p.expect(T![')']);

    if !expr(p) {
        // test_err(expr) prefix_let_expr_no_body
        // (let ((foo 1) (bar 2)))
        p.error("Expected to find a body (expression) for the prefix let expression");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::PREFIX_LET_EXPR);
}

/// Parses a prefix letrec expression.
/// the opening paren is already consumed
fn prefix_let_rec_expr(p: &mut Parser, m: Marker) {
    assert!(p.at(T![letrec]) && p.peek_at(T!['(']));

    // test(expr) prefix_let_rec_expr
    // (letrec ((foo 1) (bar 2)) foo)
    p.bump(T![letrec]);
    p.bump(T!['(']);

    while !p.at(T![')']) && !p.at_end() {
        if !super::prefix_binding(p) {
            // test_err(expr) prefix_let_rec_expr_no_binding
            // (letrec (domain Foo) foo)
            p.err_recover(
                "expected a binding in the prefix letrec expression",
                TokenSet::new(&[T![')'], T!['(']]),
            )
        }
    }

    p.expect(T![')']);

    if !phrase(p) {
        // test_err(expr) prefix_let_rec_expr_no_body
        // (letrec ((foo 1) (bar 2)))
        p.error("Expected to find a body (phrase) for the prefix letrec expression");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::PREFIX_LET_REC_EXPR);
}

fn prefix_check_clause(p: &mut Parser) {
    if !p.at(T!['(']) {
        p.error("Expected to find a prefix check clause in parens");
        return;
    }

    let m = p.start();
    p.bump(T!['(']);

    if !phrase(p) {
        p.error("Expected to find a condition for the prefix check clause");
    }

    if !expr(p) {
        // test_err(expr) prefix_check_clause_no_expr
        // (check (B ))
        p.error("Expected to find a body for the prefix check clause");
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::CHECK_CLAUSE);
}

// test(expr) prefix_check
// (check (A B) (else D))
fn prefix_check_expr(p: &mut Parser, m: Marker) {
    assert!(p.at(T![check]) && p.peek_at(T!['(']));

    p.bump(T![check]);

    while !p.at(T![')']) && !p.at_end() {
        prefix_check_clause(p);
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::PREFIX_CHECK_EXPR);
}

// test(expr) prefix_try
// (try (A B) C D)
fn prefix_try_expr(p: &mut Parser, m: Marker) {
    assert!(p.at(T![try]) && !p.peek_at(T!['{']));

    p.bump(T![try]);

    while !p.at(T![')']) && !p.at_end() {
        if !expr(p) {
            // test_err(expr) prefix_try_expr_no_expr
            // (try domain)
            p.err_recover(
                "Expected to find an expression to try",
                EXPR_START_SET.union(TokenSet::single(T![')'])),
            );
        }
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::PREFIX_TRY_EXPR);
}

/// parses an expression with the opening paren already consumed.
/// This is primarily used to do lookahead and disambiguate the
/// correct production to parse.
pub(crate) fn opened_expr(p: &mut Parser, m: Marker) {
    #[cfg(test)]
    eprintln!("opened_expr: {:?} {:?}", p.current(), p.nth(1));

    match p.current() {
        T![')'] => {
            opened_unit_expr(p, m);
        }
        T![||] => {
            opened_or_expr(p, m);
        }
        T![&&] => {
            opened_and_expr(p, m);
        }
        T![seq] => {
            opened_seq_expr(p, m);
        }
        T![let] => {
            if p.peek_at(T!['(']) {
                prefix_let_expr(p, m);
            } else {
                opened_application_expr(p, m);
            }
        }
        T![letrec] => {
            if p.peek_at(T!['(']) {
                prefix_let_rec_expr(p, m);
            } else {
                opened_application_expr(p, m);
            }
        }
        T![check] => {
            if p.peek_at(T!['(']) {
                prefix_check_expr(p, m);
            } else {
                opened_application_expr(p, m);
            }
        }
        T![try] => {
            if !p.peek_at(T!['{']) {
                prefix_try_expr(p, m);
            } else {
                opened_application_expr(p, m);
            }
        }
        T![match] => {
            // test(expr) simple_match_expr
            // match foo { bar => (baz boo) }
            let (_, pos) = super::phrases::match_expr_or_ded_partial(
                p,
                None,
                super::phrases::MatchParseState::Match(Some(m)),
            );
            match pos {
                super::phrases::PrefixOrInfix::Prefix => {}
                super::phrases::PrefixOrInfix::Infix(m) => {
                    opened_application_expr(p, m);
                }
            }
        }
        c if PHRASE_START_SET.contains(c) => {
            opened_application_expr(p, m);
        }
        _ => {
            p.err_recover(
                "Expected to find a valid expression",
                TokenSet::new(&[T![')']]).union(PHRASE_START_SET),
            );
            m.abandon(p);
        }
    }
}

pub(crate) const EXPR_START_SET: TokenSet = TokenSet::new(&[
    IDENT,
    T!['('],
    T![?],
    T!['\''],
    T![lambda],
    T!['['],
    T![check],
    T![cell],
    T![set!],
    T![ref],
    T![make - vector],
    T![vector - sub],
    T![vector-set!],
    T![while],
    T![try],
    T![let],
    T![letrec],
    T![match],
    T![method],
    T!["|{"],
    T!["}|"],
    T![_],
])
.union(LIT_SET);

pub(crate) const EXPR_AFTER_LPAREN_SET: TokenSet = TokenSet::new(&[
    T![')'],
    T![&&],
    T![||],
    T![seq],
    T![match],
    T![check],
    T![let],
    T![letrec],
    T![lambda],
])
.union(EXPR_START_SET);

// test(expr) simple_string_expr
// "hello world"
pub(crate) fn expr(p: &mut Parser) -> bool {
    #[cfg(test)]
    eprintln!("parsing expr: {:?} {:?}", p.current(), p.nth(1));

    match p.current() {
        IDENT => {
            ident_expr(p);
        }
        c if LIT_SET.contains(c) => {
            literal_expr(p);
        }
        T!['('] => {
            let m = p.start();
            p.bump(T!['(']);
            opened_expr(p, m);
        }
        T![?] => {
            term_var_expr(p);
        }
        T!['\''] => {
            meta_ident_expr(p);
        }
        T![lambda] => {
            lambda_expr(p, false);
        }
        T!['['] => {
            list_expr(p);
        }
        T![check] => {
            check_expr(p);
        }
        T![cell] => {
            cell_expr(p);
        }
        T![set!] => {
            set_expr(p);
        }
        T![ref] => {
            ref_expr(p);
        }
        T![make - vector] => {
            make_vector_expr(p);
        }
        T![vector - sub] => {
            vector_sub_expr(p);
        }
        T![vector-set!] => {
            vector_set_expr(p);
        }
        T![while] => {
            while_expr(p);
        }
        T![try] => {
            try_expr(p);
        }
        T![let] => {
            let_expr(p);
        }
        T![letrec] => {
            let_rec_expr(p);
        }
        T![match] => {
            super::phrases::match_expr_or_ded(p, Some(ExprOrDed::Expr));
        }
        T![method] => {
            method_expr(p);
        }
        T!["|{"] => {
            map_expr(p);
        }
        T![_] => {
            wildcard_expr(p);
        }
        _ => {
            return false;
        }
    }
    true
}
