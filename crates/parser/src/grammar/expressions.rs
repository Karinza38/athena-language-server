use super::{identifier, literal, patterns::pat, phrases::phrase, LIT_SET};
use crate::{
    parser::Parser,
    token_set::TokenSet,
    SyntaxKind::{self, IDENT, IDENT_EXPR, UNIT_EXPR},
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
    while phrase(p) {
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

    while phrase(p) {
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

    while phrase(p) {
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

    while phrase(p) {
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

    while phrase(p) {
        // test(expr) nested_seq_expr
        // (seq bar (seq baz))
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::SEQ_EXPR);
}

fn check_arm(p: &mut Parser) {
    assert!(p.at(T![|]));

    let m = p.start();
    p.bump(T![|]);

    if !phrase(p) {
        // test_err(expr) check_arm_no_expr
        // check { foo => bar
        // | => foo }
        p.error("Expected to find a phrase for the check arm");
    }

    p.expect(T![=>]);

    if !expr(p) {
        // test_err(expr) check_arm_no_result
        // check { foo => bar
        // | foo => }
        p.error("Expected to find a expression for the check arm result");
    }

    m.complete(p, SyntaxKind::CHECK_ARM);
}

// test(expr) simple_check_expr
// check { false => true
//      | else => false
// }
fn check_expr(p: &mut Parser) {
    assert!(p.at(T![check]));

    let m = p.start();
    p.bump(T![check]);

    p.expect(T!['{']);

    // FIXME: avoid creating an arm node if there is none
    let arm = p.start();

    if !phrase(p) {
        // test_err(expr) check_expr_no_arms
        // check {}
        p.error("Expected to find a phrase for the check expression");
    }

    p.expect(T![=>]);

    if !phrase(p) {
        // test_err(expr) check_expr_no_result_first_arm
        // check { foo => }
        p.error("Expected to find a phrase for the check expression result");
    }

    arm.complete(p, SyntaxKind::CHECK_ARM);

    while p.at(T![|]) {
        check_arm(p);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::CHECK_EXPR);
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

fn try_arm(p: &mut Parser, leading_pipe: bool) {
    let m = if leading_pipe {
        assert!(p.at(T![|]));

        let m = p.start();
        p.bump(T![|]);
        m
    } else {
        assert!(p.at_one_of(EXPR_START_SET));
        p.start()
    };

    if !expr(p) {
        // test_err(expr) try_arm_no_expr
        // try { foo |  }
        p.error("Expected to find an expression for the try arm");
    }

    m.complete(p, SyntaxKind::TRY_ARM);
}

// test(expr) simple_try_expr
// try { foo }
fn try_expr(p: &mut Parser) {
    assert!(p.at(T![try]));

    let m = p.start();
    p.bump(T![try]);

    p.expect(T!['{']);

    if !p.at_one_of(EXPR_START_SET) {
        // test_err(expr) try_expr_no_arm
        // try {  }
        p.error("Expected to find at least one arm for the try expression");
    } else {
        try_arm(p, false);
    }

    while p.at(T![|]) {
        // test(expr) try_expr_multiple_arms
        // try { foo | bar | (func baz) }
        try_arm(p, true);
    }

    p.expect(T!['}']);

    m.complete(p, SyntaxKind::TRY_EXPR);
}

fn let_part(p: &mut Parser, leading_semi: bool) {
    let m = if leading_semi {
        assert!(p.at(T![;]));

        let m = p.start();
        p.bump(T![;]);
        m
    } else {
        assert!(p.at_one_of(EXPR_START_SET));
        p.start()
    };

    if !pat(p) {
        // test_err(expr) let_part_no_pat
        // let { a := b; := c }
        p.error("Expected to find a pattern for the let binding");
    }

    p.expect(T![:=]);

    if !expr(p) {
        // test_err(expr) let_part_no_expr
        // let { foo :=   }
        p.error("Expected to find an expression for the let binding");
    }

    m.complete(p, SyntaxKind::LET_PART);
}

// test(expr) simple_let_expr
// let { foo := (hotline miami) }
fn let_expr(p: &mut Parser) {
    assert!(p.at(T![let]));

    let m = p.start();
    p.bump(T![let]);

    p.expect(T!['{']);

    if !p.at_one_of(super::patterns::PAT_START_SET) {
        // test_err(expr) let_expr_no_part
        // let {  }
        p.error("Expected to find at least one binding for the let expression");
    } else {
        let_part(p, false);
    }

    while p.at(T![;]) {
        // test(expr) let_expr_multiple_parts
        // let { foo := bar ; baz := (myfun "cool") }
        let_part(p, true);
    }

    p.expect(T!['}']);

    m.complete(p, SyntaxKind::LET_EXPR);
}

fn let_rec_part(p: &mut Parser, leading_semi: bool) {
    let m = if leading_semi {
        assert!(p.at(T![;]));

        let m = p.start();
        p.bump(T![;]);
        m
    } else {
        assert!(p.at(IDENT));
        p.start()
    };

    if !p.at(IDENT) {
        // test_err(expr) let_rec_part_no_pat
        // letrec { a := b; := c }
        p.error("Expected to find an identifier for the letrec binding");
    } else {
        identifier(p);
    }

    p.expect(T![:=]);

    if !expr(p) {
        // test_err(expr) let_rec_part_no_expr
        // letrec { foo :=   }
        p.error("Expected to find an expression for the letrec binding");
    }

    m.complete(p, SyntaxKind::LET_PART);
}

// test(expr) simple_let_rec_expr
// letrec { foo := (hotline miami) }
fn let_rec_expr(p: &mut Parser) {
    assert!(p.at(T![letrec]));

    let m = p.start();
    p.bump(T![letrec]);

    p.expect(T!['{']);

    if !p.at(IDENT) {
        // test_err(expr) let_rec_expr_no_binding
        // letrec {  }
        p.error("Expected to find at least one binding for the letrec expression");
    } else {
        let_rec_part(p, false);
    }

    while p.at(T![;]) {
        // test(expr) letrec_expr_multiple_bindings
        // letrec { foo := bar ; baz := (myfun "cool") }
        let_rec_part(p, true);
    }

    p.expect(T!['}']);

    m.complete(p, SyntaxKind::LET_REC_EXPR);
}

// test(expr) simple_match_expr
// match foo { bar => (baz boo) }
fn match_expr(p: &mut Parser) {
    super::phrases::match_expr_or_ded(p, Some(super::phrases::ExprOrDed::Expr));
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
])
.union(LIT_SET);

pub(crate) const EXPR_AFTER_LPAREN_SET: TokenSet =
    TokenSet::new(&[T![')'], T![&&], T![||], T![seq]]).union(EXPR_START_SET);

// test(expr) simple_string_expr
// "hello world"
pub(crate) fn expr(p: &mut Parser) -> bool {
    eprintln!("parsing expr: {:?} {:?}", p.current(), p.nth(1));

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
    } else if p.at(T![check]) {
        // FIXME: have to figure out how to handle ambiguity
        // between check expr and check ded
        check_expr(p);
    } else if p.at(T![cell]) {
        cell_expr(p);
    } else if p.at(T![set!]) {
        set_expr(p);
    } else if p.at(T![ref]) {
        ref_expr(p);
    } else if p.at(T![make - vector]) {
        make_vector_expr(p);
    } else if p.at(T![vector - sub]) {
        vector_sub_expr(p);
    } else if p.at(T![vector-set!]) {
        vector_set_expr(p);
    } else if p.at(T![while]) {
        while_expr(p);
    } else if p.at(T![try]) {
        try_expr(p);
    } else if p.at(T![let]) {
        // FIXME: have to figure out how to handle ambiguity
        // between let expr and let ded
        let_expr(p);
    } else if p.at(T![letrec]) {
        // FIXME: have to figure out how to handle ambiguity
        // between letrec expr and letrec ded
        let_rec_expr(p);
    } else if p.at(T![match]) {
        // FIXME: have to figure out how to handle ambiguity
        // between match expr and match ded
        match_expr(p);
    } else {
        // todo!();
        return false;
    }
    true
}
