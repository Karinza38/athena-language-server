use crate::{
    grammar::{expressions::expr, phrases::phrase},
    parser::{Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, IDENT},
    T,
};

use super::identifier;

fn method_call_ded(p: &mut Parser, kind: SyntaxKind) -> Marker {
    assert!(p.at(T!['(']) && p.peek_at(kind));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(kind);

    // test_err(ded) method_call_no_method
    // (apply-method )

    // test_err(ded) bang_method_call_no_method
    // (! )
    if !expr(p) {
        p.error("expected method expression to apply");
    }

    while !p.at(T![')']) && phrase(p) {
        // do nothing
    }

    p.expect(T![')']);
    m
}

// test(ded) apply_method_call_ded
// (apply-method claim A)
fn apply_method_call_ded(p: &mut Parser) {
    let m = method_call_ded(p, T![apply - method]);
    m.complete(p, SyntaxKind::METHOD_CALL_DED);
}

// test(ded) bang_method_call_ded
// (!claim A)
fn bang_method_call_ded(p: &mut Parser) {
    let m = method_call_ded(p, T![!]);
    m.complete(p, SyntaxKind::BANG_METHOD_CALL_DED);
}

// test(ded) assume_ded
// assume A (!claim A)
fn assume_ded(p: &mut Parser) {
    assert!(p.at(T![assume]));

    let m = p.start();
    p.bump(T![assume]);

    // test(ded) assume_nested
    // assume A assume B (!claim A)

    if !phrase(p) {
        // test_err(ded) assume_no_assumption
        // assume
        p.error("expected assumption");
    }

    if !ded(p) {
        // test_err(ded) assume_no_body
        // assume a
        p.error("expected body for assumption");
    }

    m.complete(p, SyntaxKind::ASSUME_DED);
}

fn assume_part(p: &mut Parser, leading_semi: bool) {
    let m = if leading_semi {
        let m = p.start();
        p.expect(T![;]);
        m
    } else {
        p.start()
    };

    if !p.at(IDENT) {
        // test_err(ded) assume_part_no_ident
        // assume := b
        p.error("expected identifier in assume binding");
    } else {
        identifier(p);
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(ded) assume_part_no_phrase
        // assume a :=
        p.error("expected phrase in assume binding");
    }
    m.complete(p, SyntaxKind::ASSUME_PART);
}

fn named_assume_ded(p: &mut Parser) {
    assert!(p.at(T![assume]) && p.peek_at(IDENT) && p.nth_at(2, T![:=]));

    let m = p.start();
    p.bump(T![assume]);

    assume_part(p, false);

    // test(ded) named_assume_multi_part
    // assume A := (!claim B); C := D (!claim A)
    while p.at(T![;]) {
        assume_part(p, true);
    }

    if !ded(p) {
        // test_err(ded) named_assume_no_body
        // assume a := b
        p.error("expected body for assumption");
    }

    m.complete(p, SyntaxKind::NAMED_ASSUME_DED);
}

// test(ded) match_ded
// match A { B => (!claim C) }
fn match_ded(p: &mut Parser) {
    super::phrases::match_expr_or_ded(p, Some(super::phrases::ExprOrDed::Ded));
}

// test(ded) check_ded
// check { false => (!claim A) | else => (!claim B) }
fn check_ded(p: &mut Parser) {
    super::phrases::check_expr_or_ded(p, Some(super::phrases::ExprOrDed::Ded));
}

fn let_ded(p: &mut Parser) {
    super::phrases::let_expr_or_ded(p, Some(super::phrases::ExprOrDed::Ded));
}

fn let_rec_ded(p: &mut Parser) {
    super::phrases::let_rec_expr_or_ded(p, Some(super::phrases::ExprOrDed::Ded));
}

fn try_ded(p: &mut Parser) {
    super::phrases::try_expr_or_ded(p, Some(super::phrases::ExprOrDed::Ded));
}

// test(ded) proof_by_contra_ded
// suppose-absurd A (!claim A)
fn proof_by_contra_ded(p: &mut Parser) {
    assert!(p.at(T![suppose - absurd]));

    let m = p.start();
    p.bump(T![suppose - absurd]);

    if !phrase(p) {
        // test_err(ded) proof_by_contra_no_phrase
        // suppose-absurd
        p.error("expected absurd hypothesis for proof by contradiction");
    }

    if !ded(p) {
        // test_err(ded) proof_by_contra_no_body
        // suppose-absurd A
        p.error("expected body for proof by contradiction");
    }

    m.complete(p, SyntaxKind::PROOF_BY_CONTRA_DED);
}

pub(crate) const DED_START_SET: TokenSet = TokenSet::new(&[
    T!['('],
    T![assume],
    T![match],
    T![check],
    T![let],
    T![letrec],
    T![try],
]);

pub(crate) const DED_AFTER_LPAREN_SET: TokenSet = TokenSet::new(&[T![apply - method], T![!]]);

pub(crate) fn ded(p: &mut Parser) -> bool {
    eprintln!("ded: {:?} {:?}", p.current(), p.nth(1));
    if p.at(T!['(']) {
        if p.peek_at(T![apply - method]) {
            apply_method_call_ded(p);
        } else if p.peek_at(T![!]) {
            bang_method_call_ded(p);
        } else {
            return false;
        }
    } else if p.at(T![assume]) {
        // might need to be smarter to handle missing identifier
        if p.peek_at(IDENT) && p.nth_at(2, T![:=]) {
            named_assume_ded(p);
        } else {
            assume_ded(p);
        }
    } else if p.at(T![match]) {
        match_ded(p);
    } else if p.at(T![check]) {
        check_ded(p);
    } else if p.at(T![let]) {
        let_ded(p);
    } else if p.at(T![letrec]) {
        let_rec_ded(p);
    } else if p.at(T![try]) {
        try_ded(p);
    } else if p.at(T![suppose - absurd]) {
        proof_by_contra_ded(p);
    } else {
        return false;
    }
    true
}
