use crate::grammar::expressions::expr;
use crate::grammar::{literal, meta_ident, unit};
use crate::parser::Parser;
use crate::token_set::TokenSet;
use crate::SyntaxKind::{self, IDENT};
use crate::T;

use super::sorts::sort;
use super::{identifier, LIT_SET};

// test(pat) simple_ident_pat
// foopat
fn ident_pat(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    identifier(p);
    m.complete(p, SyntaxKind::IDENT_PAT);
}

// test(pat) simple_annotated_ident_pat
// foo:Int
fn annotated_ident_pat(p: &mut Parser) {
    assert!(p.at(IDENT));
    assert!(p.peek_at(T![:]));

    let m = p.start();
    identifier(p);
    p.bump(T![:]);
    if !sort(p) {
        // test_err(pat) annotated_ident_pat_no_sort
        // foo:
        p.error("expected a sort annotation");
    }
    m.complete(p, SyntaxKind::ANNOTATED_IDENT_PAT);
}

// test(pat) simple_var_pat
// ?foo:Int
fn var_pat(p: &mut Parser) {
    assert!(p.at(T![?]));

    let m = p.start();
    p.bump(T![?]);

    if p.at(IDENT) {
        identifier(p);
    } else {
        // test_err(pat) var_pat_no_ident
        // ? :Int
        p.error("expected an identifier");
    }

    p.expect(T![:]);

    if !sort(p) {
        // test_err(pat) var_pat_no_sort
        // ?foo:
        p.error("expected a sort annotation");
    }

    m.complete(p, SyntaxKind::VAR_PAT);
}

// test(pat) simple_literal_pat
// "hello world"
fn literal_pat(p: &mut Parser) {
    assert!(p.at_one_of(LIT_SET));

    let m = p.start();
    literal(p);
    m.complete(p, SyntaxKind::LITERAL_PAT);
}

// test(pat) simple_meta_ident_pat
// 'foo
fn meta_ident_pat(p: &mut Parser) {
    assert!(p.at(T!['\'']));

    let m = p.start();
    meta_ident(p);

    m.complete(p, SyntaxKind::META_IDENT_PAT);
}

// test(pat) unit_pat
// ()
fn unit_pat(p: &mut Parser) {
    assert!(p.at(T!['(']));
    assert!(p.peek_at(T![')']));

    let m = p.start();
    unit(p);
    m.complete(p, SyntaxKind::UNIT_PAT);
}

// test(pat) wildcard_pat
// _
fn wildcard_pat(p: &mut Parser) {
    assert!(p.at(T![_]));

    let m = p.start();
    p.bump(T![_]);
    m.complete(p, SyntaxKind::WILDCARD_PAT);
}

// test(pat) named_pat
// (foo as "hello")
fn named_pat(p: &mut Parser) {
    assert!(p.at(T!['(']));
    assert!(p.peek_at(IDENT) || p.peek_at(T![bind]));

    let m = p.start();
    p.bump(T!['(']);

    if p.at(IDENT) {
        identifier(p);
        p.expect(T![as]);
        if !pat(p) {
            // test_err(pat) named_as_pat_no_pat
            // (foo as )
            p.error("expected a pattern to bind to");
        }
    } else {
        assert!(p.at(T![bind]));
        p.bump(T![bind]);
        identifier(p);
        if !pat(p) {
            // test_err(pat) named_bind_pat_no_pat
            // (bind foo )
            p.error("expected a pattern to bind to");
        }
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::NAMED_PAT);
}

// test(pat) simple_pat
// (some-var _)
fn simple_pat(p: &mut Parser) {
    assert!(p.at_one_of(SIMPLE_PAT_SET));

    if p.at(IDENT) {
        ident_pat(p);
    } else if p.at(T![_]) {
        wildcard_pat(p);
    } else {
        unreachable!()
    }
}

const SIMPLE_PAT_SET: TokenSet = TokenSet::new(&[IDENT, T![_]]);

// test(pat) some_thing_pat
// (some-var a)
fn some_thing_pat(p: &mut Parser) {
    assert!(p.at(T!['(']) && p.peek_at_one_of(SOME_THING_SET));

    let m = p.start();
    p.bump(T!['(']);

    p.bump_one_of(SOME_THING_SET);

    if p.at_one_of(SIMPLE_PAT_SET) {
        simple_pat(p)
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::SOME_THING_PAT);
}

const SOME_THING_SET: TokenSet = TokenSet::new(&[
    T![some - var],
    T![some - sent - con],
    T![some - quant],
    T![some - term],
    T![some - atom],
    T![some - sentence],
    T![some - list],
    T![some - cell],
    T![some - vector],
    T![some - proc],
    T![some - method],
    T![some - symbol],
    T![some - table],
    T![some - map],
    T![some - sub],
    T![some - char],
]);

fn val_of_pat(p: &mut Parser) {
    assert!(p.at(T!['(']) && p.peek_at(T![val - of]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![val - of]);

    if !p.at(IDENT) {
        // test_err(pat) val_of_pat_no_pat
        // (val-of )
        p.error("expected an identifier as part of the `val-of` pattern");
    } else {
        identifier(p);
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::VAL_OF_PAT);
}

// parses a rule of the form ('keyword' Pat Pat)
fn two_pat_pat(p: &mut Parser, kw: SyntaxKind, node: SyntaxKind) {
    assert!(p.at(T!['(']) && p.peek_at(kw));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(kw);

    if !pat(p) {
        // test_err(pat) two_pat_pat_no_pat
        // (split )

        // test_err(pat) two_pat_pat_no_pat_2
        // (list-of pat)
        p.error("expected a pattern");
    }

    if !pat(p) {
        // test_err(pat) two_pat_pat_no_second_pat
        // (split pat )

        // test_err(pat) two_pat_pat_no_second_pat_2
        // (list-of pat )
        p.error("expected a second pattern");
    }

    p.expect(T![')']);
    m.complete(p, node);
}

// test(pat) split_pat
// (split a b)
fn split_pat(p: &mut Parser) {
    two_pat_pat(p, T![split], SyntaxKind::SPLIT_PAT);
}

// test(pat) list_of_pat
// (list-of a b)
fn list_of_pat(p: &mut Parser) {
    two_pat_pat(p, T![list - of], SyntaxKind::LIST_OF_PAT);
}

// test(pat) list_pat
// [a b c]

// test(pat) list_pat_empty
// []
fn list_pat(p: &mut Parser) {
    assert!(p.at(T!['[']));

    let m = p.start();
    p.bump(T!['[']);

    while pat(p) {
        // test(pat) list_pat_multi_item
        // [a b [c] [] [1 2 3]]
    }

    // test_err(pat) list_pat_no_end
    // [a b c
    p.expect(T![']']);
    m.complete(p, SyntaxKind::LIST_PAT);
}

// test(pat) compound_pat
// (a b c)

// test(pat) where_pat
// ( a where b )
fn compound_or_where_pat(p: &mut Parser) {
    assert!(p.at(T!['(']) && p.peek_at_one_of(PAT_START_SET));

    let m = p.start();
    p.bump(T!['(']);

    pat(p);

    if p.at(T![where]) {
        p.bump(T![where]);
        if !expr(p) {
            // test_err(pat) where_pat_no_expr
            // (a where )
            p.error("expected an where clause expression");
        }
        p.expect(T![')']);
        m.complete(p, SyntaxKind::WHERE_PAT);
    } else {
        while pat(p) {
            // test(pat) compound_pat_multi_item
            // (a b [c] [] [1 2 3])
        }
        p.expect(T![')']);
        m.complete(p, SyntaxKind::COMPOUND_PAT);
    }
}

pub(crate) const PAT_START_SET: TokenSet =
    TokenSet::new(&[IDENT, T![?], T!['\''], T![_], T!['('], T![bind], T!['[']]).union(LIT_SET);

pub(crate) fn pat(p: &mut Parser) -> bool {
    if p.at(IDENT) {
        if p.peek_at(T![:]) {
            annotated_ident_pat(p);
        } else {
            ident_pat(p);
        }
    } else if p.at(T![?]) {
        var_pat(p);
    } else if p.at_one_of(LIT_SET) {
        literal_pat(p);
    } else if p.at(T!['\'']) {
        meta_ident_pat(p);
    } else if p.at(T!['(']) {
        if p.peek_at(T![')']) {
            unit_pat(p);
        } else if p.peek_at(IDENT) {
            if p.nth_at(2, T![as]) {
                named_pat(p);
            } else {
                compound_or_where_pat(p);
            }
        } else if p.peek_at(T![bind]) {
            named_pat(p);
        } else if p.peek_at_one_of(SOME_THING_SET) {
            some_thing_pat(p);
        } else if p.peek_at(T![val - of]) {
            val_of_pat(p);
        } else if p.peek_at(T![list - of]) {
            list_of_pat(p);
        } else if p.peek_at(T![split]) {
            split_pat(p);
        } else if p.peek_at_one_of(PAT_START_SET) {
            compound_or_where_pat(p);
        } else {
            return false;
        }
    } else if p.at(T![_]) {
        wildcard_pat(p);
    } else if p.at(T!['[']) {
        list_pat(p);
    } else {
        return false;
    }

    true
}
