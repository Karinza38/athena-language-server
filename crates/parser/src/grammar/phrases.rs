use crate::{
    grammar::{deductions::DED_AFTER_LPAREN_SET, expressions::EXPR_AFTER_LPAREN_SET},
    parser::Parser,
    token_set::TokenSet,
    SyntaxKind, T,
};

use super::{
    deductions::{ded, DED_START_SET},
    expressions::{expr, EXPR_START_SET},
    patterns::pat,
};

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub(crate) enum ExprOrDed {
    Expr,
    Ded,
    Ambig,
}

fn match_arm(p: &mut Parser, leading_pipe: bool, want: Option<ExprOrDed>) -> ExprOrDed {
    let m = if leading_pipe {
        assert!(p.at(T![|]));

        let m = p.start();
        p.bump(T![|]);
        m
    } else {
        // FIXME: Consider this assertion?
        // assert!(p.at_one_of(super::patterns::PAT_START_SET));
        p.start()
    };

    if !pat(p) {
        // test_err(expr) match_arm_no_pat
        // match foo { => bar }
        p.error("Expected to find a pattern for the match arm");
    }

    p.expect(T![=>]);

    match expr_or_ded(p) {
        Some(ExprOrDed::Expr) => {
            m.complete(p, SyntaxKind::MATCH_ARM);
            ExprOrDed::Expr
        }
        Some(ExprOrDed::Ded) => {
            m.complete(p, SyntaxKind::MATCH_DED_ARM);
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
                m.complete(p, SyntaxKind::MATCH_ARM);
                ExprOrDed::Expr
            } else {
                p.error("Expected to find a deduction for the match arm");
                m.complete(p, SyntaxKind::MATCH_DED_ARM);
                ExprOrDed::Ded
            }
        }
    }
}

pub(crate) fn match_expr_or_ded(p: &mut Parser, want: Option<ExprOrDed>) -> ExprOrDed {
    assert!(p.at(T![match]));

    let m = p.start();
    p.bump(T![match]);

    if !phrase(p) {
        // test_err(expr) match_expr_no_scrutinee
        // match { foo => bar }
        p.error("Expected to find a scrutinee (phrase) for the match expression");
    }

    p.expect(T!['{']);

    let res = if !p.at_one_of(super::patterns::PAT_START_SET) {
        // test_err(expr) match_expr_no_arm
        // match foo {  }
        if !p.at(T![=>]) {
            p.error("Expected at least one arm in the match expression");
            want.unwrap_or(ExprOrDed::Ambig)
        } else {
            match_arm(p, false, want)
        }
    } else {
        match_arm(p, false, want)
    };

    while p.at(T![|]) {
        // test(expr) match_expr_multiple_arms
        // match foo { bar => baz | qux => (quux "cool") }
        match_arm(p, true, want);
    }

    p.expect(T!['}']);

    let node_from_res = match res {
        ExprOrDed::Expr => SyntaxKind::MATCH_EXPR,
        ExprOrDed::Ded => SyntaxKind::MATCH_DED,
        ExprOrDed::Ambig => SyntaxKind::MATCH_EXPR,
    };

    match want {
        Some(ExprOrDed::Expr) => {
            m.complete(p, SyntaxKind::MATCH_EXPR);
        }
        Some(ExprOrDed::Ded) => {
            m.complete(p, SyntaxKind::MATCH_DED);
        }
        Some(ExprOrDed::Ambig) => {
            m.complete(p, node_from_res);
        }
        None => {
            m.complete(p, node_from_res);
        }
    }

    res
}

fn expr_or_ded(p: &mut Parser) -> Option<ExprOrDed> {
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
        eprintln!("Ambiguous phrase start: {:?}", p.current());
        match (p.current(), p.nth(1)) {
            (T![match], _) => {
                return Some(match_expr_or_ded(p, None));
            }
            (T!['('], peek) if DED_AFTER_LPAREN_SET.contains(peek) => {
                if !ded(p) {
                    p.error("expected deduction");
                }
                return Some(ExprOrDed::Ded);
            }
            (T!['('], peek) if EXPR_AFTER_LPAREN_SET.contains(peek) => {
                if !expr(p) {
                    p.error("expected expression");
                    return Some(ExprOrDed::Ambig);
                }
                return Some(ExprOrDed::Expr);
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

pub(crate) fn phrase(p: &mut Parser) -> bool {
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
