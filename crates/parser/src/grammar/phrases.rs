use crate::{
    grammar::{
        deductions::DED_AFTER_LPAREN_SET, expressions::EXPR_AFTER_LPAREN_SET,
        patterns::PAT_START_SET,
    },
    parser::{Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, IDENT},
    T,
};

use super::{
    deductions::{ded, DED_START_SET},
    expressions::{expr, EXPR_START_SET},
    name,
    patterns::pat,
};

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub(crate) enum ExprOrDed {
    Expr,
    Ded,
    Ambig,
}

fn expr_or_ded_opt_fallback(
    p: &mut Parser,
    m: crate::parser::Marker,
    to_node: impl Fn(ExprOrDed) -> SyntaxKind,
    want: Option<ExprOrDed>,
    actual: Option<ExprOrDed>,
    ctx: Option<&str>,
) -> ExprOrDed {
    let ded_node = to_node(ExprOrDed::Ded);
    let expr_node = to_node(ExprOrDed::Expr);
    let res = match actual {
        Some(ExprOrDed::Expr) => {
            m.complete(p, expr_node);
            ExprOrDed::Expr
        }
        Some(ExprOrDed::Ded) => {
            m.complete(p, ded_node);
            ExprOrDed::Ded
        }
        Some(ExprOrDed::Ambig) => {
            m.abandon(p);
            ExprOrDed::Ambig
        }
        None => {
            // test_err(expr) match_arm_no_expr
            // match foo { bar => }
            if want == Some(ExprOrDed::Expr) {
                p.error("Expected to find an expression for the match arm");
                m.complete(p, expr_node);
                ExprOrDed::Expr
            } else {
                p.error("Expected to find a deduction for the match arm");
                m.complete(p, ded_node);
                ExprOrDed::Ded
            }
        }
    };

    if want.is_some() && actual.is_some() && actual.unwrap() != want.unwrap() {
        let ctx = match ctx {
            Some(ctx) => format!(" for {}", ctx),
            None => String::new(),
        };
        p.error(format!(
            "Expected to find {}{}",
            match want.unwrap() {
                ExprOrDed::Expr => "an expression",
                ExprOrDed::Ded => "a deduction",
                ExprOrDed::Ambig => unreachable!(),
            },
            ctx
        ));
    }

    res
}

fn expr_or_ded_fallback(
    p: &mut Parser,
    m: crate::parser::Marker,
    to_node: impl Fn(ExprOrDed) -> SyntaxKind,
    want: Option<ExprOrDed>,
    actual: ExprOrDed,
    ctx: Option<&str>,
) {
    let node_from_res = to_node(actual);

    match want {
        Some(ExprOrDed::Expr) => {
            m.complete(p, to_node(ExprOrDed::Expr));
        }
        Some(ExprOrDed::Ded) => {
            m.complete(p, to_node(ExprOrDed::Ded));
        }
        Some(ExprOrDed::Ambig) => {
            unreachable!("should never want an ambiguous expression or deduction");
        }
        None => {
            m.complete(p, node_from_res);
        }
    }

    if want.is_some() && actual != want.unwrap() {
        let ctx = match ctx {
            Some(ctx) => format!(" for {}", ctx),
            None => String::new(),
        };
        p.error(format!(
            "Expected to find {}{}",
            match want.unwrap() {
                ExprOrDed::Expr => "an expression",
                ExprOrDed::Ded => "a deduction",
                ExprOrDed::Ambig => unreachable!(),
            },
            ctx
        ));
    }
}

pub(crate) enum MatchParseState {
    /// Haven't consumed anything yet
    /// Some if we ate an opening paren
    Match(Option<Marker>),
    /// Consumed the `match` keyword
    Scrutinee(Marker, Marker),
    /// Consumed the scrutinee
    Clauses(Marker, Marker),
}

pub(crate) enum PrefixOrInfix {
    Prefix,
    Infix(Marker),
}

pub(crate) fn match_expr_or_ded_partial(
    p: &mut Parser,
    want: Option<ExprOrDed>,
    state: MatchParseState,
) -> (ExprOrDed, PrefixOrInfix) {
    match state {
        MatchParseState::Match(m) => {
            let outer = if let Some(m) = m { m } else { p.start() };
            let inner = p.start();
            p.bump(T![match]);
            match_expr_or_ded_partial(p, want, MatchParseState::Scrutinee(outer, inner))
        }
        MatchParseState::Scrutinee(outer, inner) => {
            // test_err(expr) prefix_match_expr_no_scrutinee
            // (match (A B))
            if !phrase(p) {
                // test_err(expr) match_expr_no_scrutinee
                // match { foo => bar }
                p.error("Expected to find a scrutinee (phrase) for the match expression");
            }
            match_expr_or_ded_partial(p, want, MatchParseState::Clauses(outer, inner))
        }
        MatchParseState::Clauses(outer, inner) => {
            if p.eat(T!['{']) {
                let res = match_arm(p, want);

                while p.at_contextual_kw(T![|]) {
                    // test(expr) match_expr_multiple_arms
                    // match foo { bar => baz | qux => (quux "cool") }
                    match_arm(p, want);
                }

                p.expect(T!['}']);
                expr_or_ded_fallback(
                    p,
                    inner,
                    |eod| match eod {
                        ExprOrDed::Expr | ExprOrDed::Ambig => SyntaxKind::INFIX_MATCH_EXPR,
                        ExprOrDed::Ded => SyntaxKind::INFIX_MATCH_DED,
                    },
                    want,
                    res,
                    None,
                );
                (res, PrefixOrInfix::Infix(outer))
            } else if p.at(T!['(']) {
                while !p.at(T![')']) && !p.at_end() {
                    super::expressions::prefix_match_clause(p);

                    if p.step_count() > 100 {
                        p.err_and_bump("infinite loop in match expression");
                    }
                }

                p.expect(T![')']);
                inner.abandon(p);

                outer.complete(p, SyntaxKind::PREFIX_MATCH_EXPR);
                (ExprOrDed::Expr, PrefixOrInfix::Prefix)
            } else {
                // test_err(expr) match_no_clauses
                // match foo
                p.error("Expected to find clauses for the match");
                inner.complete(p, SyntaxKind::INFIX_MATCH_EXPR);
                (ExprOrDed::Expr, PrefixOrInfix::Infix(outer))
            }
        }
    }
}

pub(crate) fn match_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    let (res, fix) = match_expr_or_ded_partial(p, want, MatchParseState::Match(None));
    match fix {
        PrefixOrInfix::Prefix => {}
        PrefixOrInfix::Infix(m) => m.abandon(p),
    }
    res
}

fn match_arm(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    let m = p.start();

    // test(expr) match_leading_pipe
    // match foo { | bar => 1 }
    p.eat_contextual_kw(T![|]);

    if !pat(p) {
        // test_err(expr) match_arm_no_pat
        // match foo { => bar }
        p.error("Expected to find a pattern for the match arm");
    }

    p.expect(T![=>]);

    let res = expr_or_ded(p);

    expr_or_ded_opt_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr | ExprOrDed::Ambig => SyntaxKind::MATCH_ARM,
            ExprOrDed::Ded => SyntaxKind::MATCH_DED_ARM,
        },
        want,
        res,
        Some("match arm"),
    )
}

fn check_arm(p: &mut Parser, leading_pipe: bool, want: Option<ExprOrDed>) -> ExprOrDed {
    let m = if leading_pipe {
        assert!(p.at_contextual_kw(T![|]));

        let m = p.start();
        p.bump_remap(T![|]);
        m
    } else {
        // FIXME: Consider this assertion?
        // assert!(p.at_one_of(super::patterns::PAT_START_SET));
        p.start()
    };

    if !phrase(p) {
        // test_err(expr) check_arm_no_phrase
        // check { foo => bar
        // | => foo }
        p.error("Expected to find a phrase for the check arm");
    }

    p.expect(T![=>]);

    let res = match expr_or_ded(p) {
        Some(res) => res,
        None => {
            // test_err(expr) check_arm_no_expr
            // check { foo => bar
            // | baz => }

            // test_err(ded) check_arm_no_ded
            // check { foo => (!claim A)
            // | baz => }

            p.error("Expected a check arm body");
            want.unwrap_or(ExprOrDed::Ambig)
        }
    };

    expr_or_ded_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr => SyntaxKind::CHECK_ARM,
            ExprOrDed::Ded => SyntaxKind::CHECK_DED_ARM,
            ExprOrDed::Ambig => SyntaxKind::CHECK_ARM,
        },
        want,
        res,
        None,
    );

    res
}

// test(expr) simple_check_expr
// check { false => true
//      | else => false
// }

// test(ded) simple_check_ded
// check { false => (!claim A)
//      | else => (!claim B)
// }
pub(crate) fn check_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    assert!(p.at(T![check]));

    let m = p.start();
    p.bump(T![check]);

    p.expect(T!['{']);

    let to_node = |eod| match eod {
        ExprOrDed::Expr => SyntaxKind::CHECK_EXPR,
        ExprOrDed::Ded => SyntaxKind::INFIX_CHECK_DED,
        ExprOrDed::Ambig => SyntaxKind::CHECK_EXPR,
    };

    if p.at(T!['}']) {
        // test_err(expr) check_expr_no_arms
        // check { }

        // test_err(ded) check_ded_no_arms
        // check { }
        p.error("Expected at least one arm in the check expression");
        let ret = want.unwrap_or(ExprOrDed::Ambig);
        m.complete(p, to_node(ret));
        return ret;
    }

    let res = check_arm(p, false, want);

    while p.at_contextual_kw(T![|]) {
        check_arm(p, true, want);
    }

    p.expect(T!['}']);

    expr_or_ded_fallback(p, m, to_node, want, res, None);

    res
}

fn let_part(p: &mut Parser, leading_semi: bool) {
    let m = if leading_semi {
        assert!(p.at(T![;]));

        let m = p.start();
        p.bump(T![;]);
        m
    } else {
        assert!(p.at_one_of(PAT_START_SET));
        p.start()
    };

    if !pat(p) {
        // test_err(expr) let_part_no_pat
        // let { a := b; := c } a
        p.error("Expected to find a pattern for the let binding");
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(expr) let_part_no_phrase
        // let { foo :=   } a
        p.error("Expected to find a phrase for the let binding");
    }

    m.complete(p, SyntaxKind::LET_PART);
}

// test(expr) simple_let_expr
// let { foo := (hotline miami) } foo

// test(expr) let_expr_with_phrase_binding
// let { foo := (!claim A) } qwer
pub(crate) fn let_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    assert!(p.at(T![let]));

    let m = p.start();
    p.bump(T![let]);

    p.expect(T!['{']);

    if !p.at_one_of(super::patterns::PAT_START_SET) {
        // test_err(expr) let_expr_no_part
        // let {  } foo
        p.error("Expected to find at least one binding for the let expression");
    } else {
        let_part(p, false);
    }

    while p.at(T![;]) {
        // test(expr) let_expr_multiple_parts
        // let { foo := bar ; baz := (myfun "cool") } qwer
        let_part(p, true);
    }

    p.expect(T!['}']);

    let res = match expr_or_ded(p) {
        Some(res) => res,
        None => {
            // test_err(expr) let_expr_no_body
            // let { a := b; c := d }

            // test_err(ded) let_ded_no_body
            // let { a := b; c := d }

            p.error("Expected a let body");
            want.unwrap_or(ExprOrDed::Ambig)
        }
    };

    expr_or_ded_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr => SyntaxKind::LET_EXPR,
            ExprOrDed::Ded => SyntaxKind::INFIX_LET_DED,
            ExprOrDed::Ambig => SyntaxKind::LET_EXPR,
        },
        want,
        res,
        None,
    );

    res
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
        // letrec { a := b; := c } foo
        p.error("Expected to find an identifier for the letrec binding");
    } else {
        name(p);
    }

    p.expect(T![:=]);

    if !expr(p) {
        // test_err(expr) let_rec_part_no_expr
        // letrec { foo :=   } foo
        p.error("Expected to find an expression for the letrec binding");
    }

    m.complete(p, SyntaxKind::LET_REC_PART);
}

// test(expr) simple_let_rec_expr
// letrec { foo := (hotline miami) } foo
pub(crate) fn let_rec_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    assert!(p.at(T![letrec]));

    let m = p.start();
    p.bump(T![letrec]);

    p.expect(T!['{']);

    if !p.at(IDENT) {
        // test_err(expr) let_rec_expr_no_binding
        // letrec {  } foo

        // test_err(ded) let_rec_ded_no_binding
        // letrec {  } (!claim A)
        p.error("Expected to find at least one binding for the letrec expression");
    } else {
        let_rec_part(p, false);
    }

    while p.at(T![;]) {
        // test(expr) letrec_expr_multiple_bindings
        // letrec { foo := bar ; baz := (myfun "cool") } foo
        let_rec_part(p, true);
    }

    p.expect(T!['}']);

    let res = match expr_or_ded(p) {
        Some(res) => res,
        None => {
            // test_err(expr) let_rec_expr_no_body
            // letrec { a := b; c := d }

            // test_err(ded) let_rec_ded_no_body
            // letrec { a := b; c := d }

            p.error("Expected a letrec body");
            want.unwrap_or(ExprOrDed::Ambig)
        }
    };

    expr_or_ded_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr => SyntaxKind::LET_REC_EXPR,
            ExprOrDed::Ded => SyntaxKind::INFIX_LET_REC_DED,
            ExprOrDed::Ambig => SyntaxKind::LET_REC_EXPR,
        },
        want,
        res,
        None,
    );

    res
}

fn try_arm(p: &mut Parser, leading_pipe: bool, want: Option<ExprOrDed>) -> ExprOrDed {
    let m = if leading_pipe {
        assert!(p.at_contextual_kw(T![|]));

        let m = p.start();
        p.bump_remap(T![|]);
        m
    } else {
        p.start()
    };

    let res = match expr_or_ded(p) {
        Some(res) => res,
        None => {
            // test_err(expr) try_arm_no_expr
            // try { foo |  }

            // test_err(ded) try_ded_no_ded
            // try { (!claim A) |  }
            p.error("Expected to find an expression for the try arm");
            want.unwrap_or(ExprOrDed::Ambig)
        }
    };

    expr_or_ded_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr => SyntaxKind::TRY_ARM,
            ExprOrDed::Ded => SyntaxKind::TRY_DED_ARM,
            ExprOrDed::Ambig => SyntaxKind::TRY_ARM,
        },
        want,
        res,
        None,
    );

    res
}

// test(expr) simple_try_expr
// try { foo }

// test(ded) simple_try_ded
// try {  (!claim A) }
pub(crate) fn try_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    assert!(p.at(T![try]));

    let m = p.start();
    p.bump(T![try]);

    p.expect(T!['{']);

    let res = if p.at(T!['}']) {
        // test_err(expr) try_expr_no_arm
        // try {  }

        // test_err(ded) try_ded_no_arm
        // try {  }
        p.error("Expected to find at least one arm for the try block");
        want.unwrap_or(ExprOrDed::Ambig)
    } else {
        try_arm(p, false, want)
    };

    while p.at_contextual_kw(T![|]) {
        // test(expr) try_expr_multiple_arms
        // try { foo | bar | (func baz) }

        // test(ded) try_ded_multiple_arms
        // try {  (!claim A) | (!claim B) | (!claim C) }
        try_arm(p, true, want);
    }

    p.expect(T!['}']);

    expr_or_ded_fallback(
        p,
        m,
        |eod| match eod {
            ExprOrDed::Expr => SyntaxKind::TRY_EXPR,
            ExprOrDed::Ded => SyntaxKind::INFIX_TRY_DED,
            ExprOrDed::Ambig => SyntaxKind::TRY_EXPR,
        },
        want,
        res,
        None,
    );

    res
}

pub(crate) fn expr_or_ded(p: &mut Parser) -> Option<ExprOrDed> {
    #[cfg(test)]
    eprintln!("parsing expr_or_ded: {:?} {:?}", p.current(), p.nth(1));
    if p.at_one_of(EXPR_START_SET.subtract(DED_START_SET)) {
        if !expr(p) {
            p.error("expected expression");
        }
        return Some(ExprOrDed::Expr);
    } else if p.at_one_of(DED_START_SET.subtract(EXPR_START_SET)) {
        if !ded(p) {
            p.error("expected deduction");
        }
        return Some(ExprOrDed::Ded);
    } else if p.at_one_of(AMBIG_START) {
        #[cfg(test)]
        eprintln!("Ambiguous phrase starty: {:?}", p.current());
        match (p.current(), p.nth(1)) {
            (T![match], _) => {
                return Some(match_expr_or_ded(p, None));
            }
            (T![check], _) => {
                return Some(check_expr_or_ded(p, None));
            }
            (T![try], _) => {
                return Some(try_expr_or_ded(p, None));
            }
            (T!['('], peek)
                if DED_AFTER_LPAREN_SET
                    .subtract(EXPR_AFTER_LPAREN_SET)
                    .contains(peek) =>
            {
                if !ded(p) {
                    p.error("expected deduction");
                }
                return Some(ExprOrDed::Ded);
            }
            (T!['('], peek) if EXPR_AFTER_LPAREN_SET.contains(peek) => {
                let m = p.start();
                p.bump(T!['(']);

                if p.at(T![by]) {
                    super::deductions::by_ded_partial(p, m);
                    return Some(ExprOrDed::Ded);
                } else {
                    super::expressions::opened_expr(p, m);
                    return Some(ExprOrDed::Expr);
                }
            }
            (T![let], T!['{']) => {
                return Some(let_expr_or_ded(p, None));
            }
            (T![let], T!['(']) => {
                return Some(let_expr_or_ded(p, None));
            }
            (T![letrec], _) => {
                return Some(let_rec_expr_or_ded(p, None));
            }
            _ => {
                p.error("Ambiguous phrase start");
                // FIXME: smarter fallback
                if !expr(p) {
                    p.error("expected expression");
                    return Some(ExprOrDed::Ambig);
                }
                return Some(ExprOrDed::Expr);
            }
        }
    }

    None
}

const AMBIG_START: TokenSet = EXPR_START_SET.intersect(DED_START_SET);

pub(crate) const PHRASE_START_SET: TokenSet = EXPR_START_SET.union(DED_START_SET);

pub(crate) fn phrase(p: &mut Parser) -> bool {
    #[cfg(test)]
    eprintln!("parsing phrase: {:?} {:?}", p.current(), p.nth(1));
    let m = p.start();

    match expr_or_ded(p) {
        Some(ExprOrDed::Expr) => {
            m.complete(p, SyntaxKind::EXPR_PHRASE);
            true
        }
        Some(ExprOrDed::Ded) => {
            m.complete(p, SyntaxKind::DED_PHRASE);
            true
        }
        Some(ExprOrDed::Ambig) => {
            p.error("Ambiguous phrase");
            m.abandon(p);
            false
        }
        None => {
            m.abandon(p);
            false
        }
    }
}
