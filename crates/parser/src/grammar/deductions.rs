use crate::{
    grammar::{expressions::expr, phrases::phrase, sorts::sort},
    parser::{Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, IDENT},
    T,
};

use super::{identifier, patterns};

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

// test(ded) generalize_over_ded
// generalize-over A (!claim A)
fn generalize_over_ded(p: &mut Parser) {
    assert!(p.at(T![generalize - over]));

    let m = p.start();
    p.bump(T![generalize - over]);

    if !expr(p) {
        // test_err(ded) generalize_over_no_expr
        // generalize-over
        p.error("expected expression in generalize over binding");
    }

    if !ded(p) {
        // test_err(ded) generalize_over_no_body
        // generalize-over a
        p.error("expected body for generalize over");
    }

    m.complete(p, SyntaxKind::GENERALIZE_OVER_DED);
}

// test(ded) pick_any_ded
// pick-any a : Int (!claim A)
fn pick_any_ded(p: &mut Parser) {
    assert!(p.at(T![pick - any]));

    let m = p.start();
    p.bump(T![pick - any]);

    if !p.at(IDENT) {
        // test_err(ded) pick_any_no_ident
        // pick-any : Int (!claim A)
        p.error("expected identifier in pick any binding");
    } else {
        identifier(p);
    }

    if p.at(T![:]) {
        p.bump(T![:]);
        if !sort(p) {
            // test_err(ded) pick_any_no_sory
            // pick-any a : assume B (!claim B)
            p.error("expected sort in pick any binding");
        }
    }

    if !ded(p) {
        // test_err(ded) pick_any_no_body
        // pick-any a : Int
        p.error("expected body for pick any");
    }

    m.complete(p, SyntaxKind::PICK_ANY_DED);
}

fn with_witness_ded(p: &mut Parser) {
    assert!(p.at(T![with - witness]));

    let m = p.start();
    p.bump(T![with - witness]);

    if !expr(p) {
        // test_err(ded) with_witness_no_witness
        // with-witness
        p.error("expected witness expression in with witness deduction");
    }

    if !phrase(p) {
        // test_err(ded) with_witness_no_phrase
        // with-witness a
        p.error("expected phrase in with witness binding");
    }

    if !ded(p) {
        // test_err(ded) with_witness_no_body
        // with-witness a b
        p.error("expected body for with witness");
    }

    m.complete(p, SyntaxKind::WITH_WITNESS_DED);
}

fn pick_witness_ded(p: &mut Parser) {
    assert!(p.at(T![pick - witness]));

    let m = p.start();
    p.bump(T![pick - witness]);

    if !p.at(IDENT) {
        // test_err(ded) pick_witness_no_witness
        // pick-witness
        p.error("expected witness name in pick witness binding");
    } else {
        identifier(p);
    }

    p.expect(T![for]);

    if !ded(p) {
        // test_err(ded) pick_witness_no_body
        // pick-witness a for
        p.error("expected body for pick witness");
    }

    m.complete(p, SyntaxKind::PICK_WITNESS_DED);
}

fn pick_witnesses_ded(p: &mut Parser) {
    assert!(p.at(T![pick - witnesses]));

    let m = p.start();
    p.bump(T![pick - witnesses]);

    if !p.at(IDENT) {
        // test_err(ded) pick_witnesses_no_witness
        // pick-witnesses
        p.error("expected at least one witness name in pick witnesses binding");
    } else {
        identifier(p);
    }

    while p.at(IDENT) {
        // test(ded) pick_witnesses_multiple
        // pick-witnesses a b c for (!claim a)
        identifier(p);
    }

    p.expect(T![for]);

    if !ded(p) {
        // test_err(ded) pick_witnesses_no_body
        // pick-witnesses a for
        p.error("expected body for pick witnesses");
    }

    m.complete(p, SyntaxKind::PICK_WITNESSES_DED);
}

// test(ded) restricted_apply_pat
// by-induction a { (foo bar) => (!claim bar) }
fn restricted_apply_pat(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    if !p.at(IDENT) {
        p.error("expected identifier in restricted apply pattern");
    } else {
        identifier(p);
    }

    restricted_pat(p);

    while !p.at(T![')']) {
        restricted_pat(p);
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::RESTRICTED_APPLY_PAT);
}

// test(ded) restricted_named_pat
// by-induction a { (foo as bar) => (!claim bar) }
fn restricted_named_pat(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(ded) restricted_named_pat_no_ident
        // by-induction a { ( as bar) => (!claim bar) }
        p.error("expected identifier in restricted named pattern");
    } else {
        identifier(p);
    }

    p.expect(T![as]);

    restricted_pat(p);

    p.expect(T![')']);

    m.complete(p, SyntaxKind::RESTRICTED_NAMED_PAT);
}

fn restricted_pat(p: &mut Parser) {
    if p.at(IDENT) {
        if p.peek_at(T![:]) {
            patterns::annotated_ident_pat(p);
        } else {
            patterns::ident_pat(p);
        }
    } else if p.at(T!['(']) {
        if p.peek_at(T![as]) || p.nth_at(2, T![as]) {
            restricted_named_pat(p);
        } else {
            restricted_apply_pat(p);
        }
    } else {
        p.error("failed to parse restricted pattern");
    }
}

fn restricted_match_arm(p: &mut Parser, leading_pipe: bool) {
    let m = if leading_pipe {
        assert!(p.at(T![|]));
        let m = p.start();
        p.bump(T![|]);
        m
    } else {
        p.start()
    };

    restricted_pat(p);

    p.expect(T![=>]);

    if !ded(p) {
        p.error("expected body for restricted match arm");
    }

    m.complete(p, SyntaxKind::RESTRICTED_MATCH_DED);
}

// test(ded) induct_ded
// by-induction foo { a => (!claim a) }
fn induct_ded(p: &mut Parser) {
    assert!(p.at(T![by - induction]));

    let m = p.start();
    p.bump(T![by - induction]);

    if !phrase(p) {
        // test_err(ded) induct_no_phrase
        // by-induction { a => (!claim a) }
        p.error("expected phrase in by-induction deduction");
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(ded) induct_empty
        // by-induction a {}
        p.error("expected at least one case in by-induction");
    } else {
        restricted_match_arm(p, false);
    }

    while p.at(T![|]) {
        // test(ded) induct_multiple
        // by-induction a { a => (!claim a) | b => (!claim b) }
        restricted_match_arm(p, true);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::INDUCT_DED);
}

// test(ded) cases_ded
// datatype-cases a { foo => (!claim foo) }
fn cases_ded(p: &mut Parser) {
    assert!(p.at(T![datatype - cases]));

    let m = p.start();
    p.bump(T![datatype - cases]);

    if !phrase(p) {
        // test_err(ded) cases_no_phrase
        // datatype-cases { a => (!claim a) }
        p.error("expected phrase in datatype-cases deduction");
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(ded) cases_empty
        // datatype-cases a {}
        p.error("expected at least one case in datatype-cases");
    } else {
        restricted_match_arm(p, false);
    }

    while p.at(T![|]) {
        // test(ded) cases_multiple
        // datatype-cases a { a => (!claim a) | b => (!claim b) }
        restricted_match_arm(p, true);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::CASES_DED);
}

pub(crate) const DED_START_SET: TokenSet = TokenSet::new(&[
    T!['('],
    T![assume],
    T![match],
    T![check],
    T![let],
    T![letrec],
    T![try],
    T![suppose - absurd],
    T![generalize - over],
    T![pick - any],
    T![with - witness],
    T![pick - witness],
    T![pick - witnesses],
    T![by - induction],
    T![datatype - cases],
]);

pub(crate) const DED_AFTER_LPAREN_SET: TokenSet = TokenSet::new(&[T![apply - method], T![!]]);

pub(crate) fn ded(p: &mut Parser) -> bool {
    #[cfg(test)]
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
    } else if p.at(T![generalize - over]) {
        generalize_over_ded(p);
    } else if p.at(T![pick - any]) {
        pick_any_ded(p);
    } else if p.at(T![with - witness]) {
        with_witness_ded(p);
    } else if p.at(T![pick - witness]) {
        pick_witness_ded(p);
    } else if p.at(T![pick - witnesses]) {
        pick_witnesses_ded(p);
    } else if p.at(T![by - induction]) {
        induct_ded(p);
    } else if p.at(T![datatype - cases]) {
        cases_ded(p);
    } else {
        return false;
    }
    true
}
