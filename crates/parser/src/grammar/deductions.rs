use crate::{
    grammar::{
        expressions::{expr, EXPR_START_SET},
        maybe_typed_param,
        phrases::phrase,
        sorts::sort,
    },
    parser::{Marker, Parser},
    token_set::TokenSet,
    SyntaxKind::{self, IDENT},
    T,
};

use super::{identifier, patterns::pat};

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

fn prefix_assume_ded(p: &mut Parser) {
    assert!(p.at_prefix_kw(T![assume]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![assume]);

    let phrase_ded_r_paren = |p: &mut Parser| {
        if !phrase(p) {
            // test_err(ded) prefix_assume_no_phrase
            // (assume )
            p.error("expected assumption (phrase) in prefix assumption");
        }
        if !ded(p) {
            // test_err(ded) prefix_assume_no_body
            // (assume a)
            p.error("expected body for assumption");
        }
        p.expect(T![')']);
    };

    if (p.at(IDENT) && p.peek_at(T![:=])) || p.at(T![:=]) {
        if p.at(T![:=]) {
            // test_err(ded) prefix_assume_no_ident
            // (assume := b (!claim C))
            p.err_and_bump("expected identifier in assume binding");
        } else {
            identifier(p);
            p.bump(T![:=]);
        }
        phrase_ded_r_paren(p);
        m.complete(p, SyntaxKind::PREFIX_NAMED_ASSUME_DED);
    } else {
        phrase_ded_r_paren(p);
        m.complete(p, SyntaxKind::PREFIX_SINGLE_ASSUME_DED);
    }
}

// test(ded) prefix_assume_let_ded
// (assume-let (x A) (!claim x))
fn prefix_assume_let_ded(p: &mut Parser) {
    assert!(p.at_prefix_kw(T![assume-let]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![assume - let]);

    if !super::prefix_binding(p) {
        // test_err(ded) prefix_assume_let_no_binding
        // (assume-let  (!claim foo))
        p.err_recover(
            "expected a binding in the assume-let deduction",
            TokenSet::new(&[T![')']]).union(DED_START_SET),
        );
    }

    if !ded(p) {
        // test_err(ded) prefix_assume_let_no_body
        // (assume-let (x A) )
        p.err_recover(
            "expected a body in the assume-let deduction",
            TokenSet::new(&[T![')']]), // FIXME: questionable
        );
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::PREFIX_ASSUME_LET_DED);
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
        if p.at(T![:]) {
            // test_err(ded) pick_any_no_ident
            // pick-any : Int (!claim A)
            let m = p.start();
            p.error("expected identifier in pick any binding");
            p.bump(T![:]);
            sort(p);
            m.complete(p, SyntaxKind::TYPED_PARAM);
        } else {
            p.err_recover(
                "expected identifier in pick any binding",
                DED_START_SET.union(TokenSet::new(&[IDENT])),
            );
        }
    } else {
        maybe_typed_param(p);
    }

    while p.at(IDENT) {
        // test(ded) pick_any_multi_param
        // pick-any f:(Fun 'S 'T) g:(Fun 'U 'S) (!claim A)
        maybe_typed_param(p);
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

    if !phrase(p) {
        // test_err(ded) pick_witness_no_phrase
        // pick-witness a for
        p.error("expected phrase in pick witness binding");
    }

    if p.at(IDENT) {
        // test(ded) pick_witness_with_ident
        // pick-witness a for b c (!claim a)
        identifier(p);
    }

    if !ded(p) {
        // test_err(ded) pick_witness_no_body
        // pick-witness a for b
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
        // pick-witnesses a b c for d (!claim a)
        identifier(p);
    }

    p.expect(T![for]);

    if !phrase(p) {
        // test_err(ded) pick_witnesses_no_phrase
        // pick-witnesses a for
        p.error("expected phrase in pick witnesses binding");
    }

    if p.at(IDENT) {
        // test(ded) pick_witnesses_with_ident
        // pick-witnesses a for b c (!claim a)
        identifier(p);
    }

    if !ded(p) {
        // test_err(ded) pick_witnesses_no_body
        // pick-witnesses a for a
        p.error("expected body for pick witnesses");
    }

    m.complete(p, SyntaxKind::PICK_WITNESSES_DED);
}

fn restricted_match_arm(p: &mut Parser, leading_pipe: bool) {
    let m = if leading_pipe {
        assert!(p.at_contextual_kw(T![|]));
        let m = p.start();
        p.bump_remap(T![|]);
        m
    } else {
        p.start()
    };

    if !pat(p) {
        p.error("expected pattern in match arm");
    }

    p.expect(T![=>]);

    if !ded(p) {
        p.error("expected body for restricted match arm");
    }

    m.complete(p, SyntaxKind::RESTRICTED_MATCH_DED);
}

// test(ded) induct_ded
// by-induction foo { a => (!claim a) }

// test(ded) induct_ded_multiple
// by-induction f_x_less_than {
//     zero => conclude base_case := (factorial (zero - one) <= factorial zero)
//                 (!force base_case)
//
//
//     | (m as (S n)) => conclude inductive_step := ((factorial (m - one)) <= (factorial m))
//                 (!force inductive_step)
// }
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

    while p.at_contextual_kw(T![|]) {
        // test(ded) induct_multiple
        // by-induction a { a => (!claim a) | b => (!claim b) }
        restricted_match_arm(p, true);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::INDUCT_DED);
}

// test(ded) cases_ded
// datatype-cases a { foo => (!claim foo) }

// test(ded) cases_infer_block
// datatype-cases { (!claim A) } { foo => (!claim foo) | bar => (!claim bar) }
fn cases_ded(p: &mut Parser) {
    assert!(p.at(T![datatype - cases]));

    let m = p.start();
    p.bump(T![datatype - cases]);

    if !phrase(p) {
        // FIXME: This error message doesn't actually happen because the block is parsed as a deduction (inference block)
        // it would be good to fix this regression

        // test_err(ded) cases_no_phrase
        // datatype-cases { a => (!claim a) }
        p.error("expected phrase in datatype-cases deduction");
    }

    if p.at(T![on]) {
        // test(ded) cases_with_on
        // datatype-cases a on b { c => (!claim d) }
        p.bump(T![on]);
        if !expr(p) {
            // test_err(ded) cases_with_on_no_term
            // datatype-cases a on { a => (!claim a) }
            p.error("expected term after `on` in datatype-cases deduction");
        }
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(ded) cases_empty
        // datatype-cases a {}
        p.error("expected at least one case in datatype-cases");
    } else {
        restricted_match_arm(p, false);
    }

    while p.at_contextual_kw(T![|]) {
        // test(ded) cases_multiple
        // datatype-cases a { a => (!claim a) | b => (!claim b) }
        restricted_match_arm(p, true);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::CASES_DED);
}

// test(ded) conclude_ded
// conclude A
//  (!claim A)
fn conclude_ded(p: &mut Parser) {
    assert!(p.at(T![conclude]));

    let m = p.start();
    p.bump(T![conclude]);

    if p.at(IDENT) && p.peek_at(T![:=]) || p.at(T![:=]) {
        // test(ded) conclude_ded_named
        // conclude base_case := (factorial (zero - one) <= factorial zero)
        //  (!force base_case)
        if !p.at(IDENT) {
            // test_err(ded) conclude_no_ident
            // conclude := (factorial (zero - one) <= factorial zero)
            //  (!force base_case)
            p.error("expected identifier in conclude deduction");
        } else {
            identifier(p);
        }
        p.expect(T![:=]);
    }

    if !expr(p) {
        // test_err(ded) conclude_no_expr
        // conclude
        p.error("expected expression yielding a sentence in conclude deduction");
    }

    if !phrase(p) {
        // test_err(ded) conclude_no_phrase
        // conclude A
        p.error("expected phrase in conclude deduction");
    }

    m.complete(p, SyntaxKind::CONCLUDE_DED);
}

// test(ded) infer_from
// { A from B }
fn infer_from(p: &mut Parser, m: Marker) {
    assert!(p.at(T![from]));

    p.expect(T![from]);

    if !phrase(p) {
        // test_err(ded) infer_from_no_phrase
        // { A from }
        p.error("expected phrase to infer from");
    }

    while p.at(T![,]) {
        // test(ded) infer_from_multiple
        // { A from B, C }
        p.bump(T![,]);
        if !phrase(p) {
            // test_err(ded) infer_from_multiple_no_phrase
            // { A from B, }
            p.error("expected phrase after comma");
        }
    }

    m.complete(p, SyntaxKind::INFER_FROM);
}

// test(ded) infer_by
// { A by B on C }
fn infer_by(p: &mut Parser, m: Marker) {
    assert!(p.at(T![by]) || p.at(T![on]));

    if p.at(T![by]) {
        p.bump(T![by]);
        if !expr(p) {
            // test_err(ded) infer_by_no_expr
            // { A by }
            p.error("expected expression to infer by");
        }
    } else {
        // test(ded) infer_by_no_by
        // { A on B }
    }

    // test_err(ded) infer_by_no_on
    // { A by B C, D }
    p.expect(T![on]);

    if !phrase(p) {
        // test_err(ded) infer_by_no_phrase
        // { A by B on }
        p.error("expected phrase to infer on");
    }

    while p.at(T![,]) {
        // test(ded) infer_by_multiple
        // { A by B on C, D }
        p.bump(T![,]);
        if !phrase(p) {
            // test_err(ded) infer_by_multiple_no_phrase
            // { A by B on C, }
            p.error("expected phrases after comma");
        }
    }

    m.complete(p, SyntaxKind::INFER_BY);
}

fn maybe_named_inference(p: &mut Parser) -> bool {
    let m = p.start();

    let mut named = false;
    if p.at_one_of(NAMED_INFER_START) {
        // test(ded) named_inference
        // { A := (!claim B) }
        for n in 1..=6 {
            // 6 is the max lookahead for `:=`. Minimum is 1 (e.g. `a := ...`). Max is 6 (e.g. `a:(OP 2) := ...`)
            // Instead of just checking 1 & 6, we check all of them in case there is an error in the input as in the
            // following test case:

            // test_err(ded) named_inference_err_lookahead
            // { A:(OP) := (!claim B) }

            // test(ded) named_inference_closest_lookahead
            // { A := (!claim B) }

            // test(ded) named_inference_farthest_lookahead
            // { A:(OP 2) := (!claim B) }
            if p.nth_at(n, T![:=]) {
                maybe_wildcard_op_annotated_param(p);
                p.expect(T![:=]);
                named = true;
                break;
            }
        }
    }

    if !inference(p, named) {
        // test_err(ded) named_inference_no_inference
        // { A := }
        m.abandon(p);
        return false;
    } else {
        m.complete(p, SyntaxKind::MAYBE_NAMED_INFERENCE);
        true
    }
}

const NAMED_INFER_START: TokenSet = TokenSet::new(&[IDENT, T![_]]);

fn maybe_wildcard_op_annotated_param(p: &mut Parser) {
    let m = p.start();
    if p.at(T![_]) {
        // test(ded) wildcard_param
        // { _ := (!claim B) }
        p.bump(T![_]);
    } else if p.at(IDENT) {
        if p.peek_at(T![:]) {
            // test(ded) maybe_wildcard_op_annotated_param
            // { A:(OP 2) := (!claim B) }
            super::op_annotated_param(p);
        } else {
            // test(ded) maybe_wildcard_op_annotated_param_no_op
            // { A := (!claim B) }
            identifier(p);
        }
    }
    m.complete(p, SyntaxKind::MAYBE_WILDCARD_OP_ANNOTATED_PARAM);
}

fn inference(p: &mut Parser, allow_expr: bool) -> bool {
    let m = p.start();
    let m2 = p.start();
    if let Some(res) = super::phrases::expr_or_ded(p) {
        match res {
            super::phrases::ExprOrDed::Expr => {
                if p.at(T![from]) {
                    infer_from(p, m2);
                } else if p.at(T![by]) || p.at(T![on]) {
                    infer_by(p, m2);
                } else {
                    if allow_expr {
                        // test(ded) inference_no_from_by_on
                        // { A := B }
                        m2.abandon(p);
                        m.abandon(p);
                        return true;
                    }
                    // test_err(ded) inference_no_from_by_on
                    // { A }
                    m2.abandon(p);
                    p.error("expected `from` or `by` or `on`");
                    m.abandon(p);
                    return false;
                }
                m.complete(p, SyntaxKind::INFERENCE);
            }
            super::phrases::ExprOrDed::Ded => {
                m2.abandon(p);
                m.complete(p, SyntaxKind::INFERENCE);
            }
            super::phrases::ExprOrDed::Ambig => {
                // not actually sure how to hit this case
                p.error("ambiguous expression or deduction");
                m2.abandon(p);
                m.complete(p, SyntaxKind::INFERENCE);
            }
        }
    } else {
        // test_err(ded) inference_no_expr_ded
        // { define foo := 1 }

        p.error("expected an inference");
        m2.abandon(p);
        m.abandon(p);
        return false;
    }
    true
}

// FIXME: everything around inference blocks feels like a mess

// test(ded) infer_block_ded
// {
//   foo := A from B, C;
//   bar := D by E on F, G;
//   baz:(OP 2) := (!claim X);
//   (!claim Z)
//}
fn infer_block_ded(p: &mut Parser) -> bool {
    assert!(p.at(T!['{']) || p.at(T![begin]));

    let m = p.start();
    let close = if p.at(T!['{']) {
        p.bump(T!['{']);
        T!['}']
    } else {
        p.bump(T![begin]);
        T![end]
    };

    if !maybe_named_inference(p) {
        p.error("expected at least one inference");
        m.abandon(p);
        return false;
    }

    while !p.at(close) && !p.at_end() {
        p.eat(T![;]);
        if !maybe_named_inference(p) {
            p.err_recover(
                "expected an inference",
                DED_START_SET
                    .union(EXPR_START_SET)
                    .union(TokenSet::new(&[close])),
            );
        }
    }

    p.expect(close);

    m.complete(p, SyntaxKind::INFER_BLOCK_DED);
    true
}

pub(crate) fn by_ded_partial(p: &mut Parser, m: Marker) {
    assert!(p.at(T![by]));

    p.bump(T![by]);

    if !ded(p) {
        // test_err(ded) by_ded_partial_no_ded
        // (A by )
        p.error("expected a deduction");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::BY_DED);
}

// test(ded) by_ded
// (A by (!claim B))
fn by_ded(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    if !phrase(p) {
        // test_err(ded) by_ded_no_expr
        // ( by (!claim A))
        p.error("expected an expression");
    }

    if p.at(T![by]) {
        by_ded_partial(p, m);
    } else {
        // test_err(ded) by_ded_no_by
        // (B (!claim A))
        p.error("expected to find `by`");
        if !ded(p) {
            // test_err(ded) by_ded_no_ded
            // (B )
            p.error("expected a deduction");
        }
        p.expect(T![')']);
        m.complete(p, SyntaxKind::BY_DED);
    }
}

fn prefix_match_ded_clause(p: &mut Parser) -> bool {
    if !p.at(T!['(']) {
        p.error("Expected to find a prefix match clause in parens");
        return false;
    }

    let m = p.start();
    p.bump(T!['(']);

    if !pat(p) {
        // test_err(ded) prefix_match_ded_clause_no_pat
        // (dmatch foo ( (B by (!claim C))))

        // FIXME: this error message doesn't really get hit in the case above. It's rough though
        p.error("Expected to find a pattern for the prefix match clause");
    }

    if !ded(p) {
        // test_err(ded) prefix_match_ded_clause_no_ded
        // (dmatch foo (B ))
        p.error("Expected to find an deduction for the prefix match clause");
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::MATCH_DED_CLAUSE);

    true
}

// test(ded) prefix_match_ded
// (dmatch foo (A (!claim B)) (C (!claim D)))
fn prefix_match_ded(p: &mut Parser) {
    assert!(p.at_prefix_kw(T![dmatch]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![dmatch]);

    if !phrase(p) {
        // test_err(ded) prefix_match_ded_no_scrutinee
        // (dmatch )
        p.error("Expected to find a scrutinee (phrase) for the prefix match deduction");
    }

    if !p.at(T!['(']) {
        // test_err(ded) prefix_match_ded_no_clauses
        // (dmatch foo )
        p.error("Expected to find a prefix match clause in parens");
        p.expect(T![')']);
        m.complete(p, SyntaxKind::PREFIX_MATCH_DED);
        return;
    }

    while !p.at(T![')']) && !p.at_end() {
        if !prefix_match_ded_clause(p) {
            p.err_recover("Invalid prefix match clause", TokenSet::new(&[T![')']]));
        }
    }

    p.expect(T![')']); // end of dmatch

    m.complete(p, SyntaxKind::PREFIX_MATCH_DED);
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
    T![conclude],
    T!['{'],
    T![begin],
]);

pub(crate) const DED_AFTER_LPAREN_SET: TokenSet = TokenSet::new(&[
    T![apply - method],
    T![!],
    T![dmatch],
    T![assume],
    T![assume-let],
])
.union(EXPR_START_SET);

pub(crate) fn ded(p: &mut Parser) -> bool {
    #[cfg(test)]
    eprintln!("ded: {:?} {:?}", p.current(), p.nth(1));
    match p.current() {
        T!['('] => match p.nth(1) {
            T![apply - method] => {
                apply_method_call_ded(p);
            }
            T![!] => {
                bang_method_call_ded(p);
            }
            T![dmatch] => {
                prefix_match_ded(p);
            }
            pk if EXPR_START_SET.contains(pk) || pk == T![by] => {
                by_ded(p);
            }
            T![assume] => {
                prefix_assume_ded(p);
            }
            T![assume-let] => {
                prefix_assume_let_ded(p);
            }
            _ => {
                return false;
            }
        },
        T![assume] => {
            // might need to be smarter to handle missing identifier
            if p.peek_at(IDENT) && p.nth_at(2, T![:=]) {
                named_assume_ded(p);
            } else {
                assume_ded(p);
            }
        }
        T![match] => {
            match_ded(p);
        }
        T![check] => {
            check_ded(p);
        }
        T![let] => {
            let_ded(p);
        }
        T![letrec] => {
            let_rec_ded(p);
        }
        T![try] => {
            try_ded(p);
        }
        T![suppose - absurd] => {
            proof_by_contra_ded(p);
        }
        T![generalize - over] => {
            generalize_over_ded(p);
        }
        T![pick - any] => {
            pick_any_ded(p);
        }
        T![with - witness] => {
            with_witness_ded(p);
        }
        T![pick - witness] => {
            pick_witness_ded(p);
        }
        T![pick - witnesses] => {
            pick_witnesses_ded(p);
        }
        T![by - induction] => {
            induct_ded(p);
        }
        T![datatype - cases] => {
            cases_ded(p);
        }
        T![conclude] => {
            conclude_ded(p);
        }
        T!['{'] | T![begin] => {
            if !infer_block_ded(p) {
                return false;
            }
        }
        _ => return false,
    }

    true
}
