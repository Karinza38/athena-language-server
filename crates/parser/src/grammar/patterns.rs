use crate::parser::Parser;
use crate::token_set::TokenSet;
use crate::SyntaxKind::{self, IDENT};
use crate::T;

use super::identifier;
use super::sorts::sort;

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
    assert!(p.nth_at(1, T![:]));

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

pub(crate) const PAT_START_SET: TokenSet = TokenSet::new(&[IDENT]);

pub(crate) fn pat(p: &mut Parser) -> bool {
    if p.at(IDENT) {
        if p.nth_at(1, T![:]) {
            annotated_ident_pat(p);
        } else {
            ident_pat(p);
        }
    } else {
        return false;
    }

    true
}
