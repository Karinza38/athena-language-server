mod expressions;
mod patterns;
mod phrases;
mod sorts;

use crate::{parser::Parser, token_set::TokenSet, SyntaxKind, T};
use SyntaxKind::{CHAR, IDENT, STRING};

pub(crate) mod entry {
    use super::*;

    pub(crate) fn expr(p: &mut Parser) {
        let m = p.start();
        super::expressions::expr(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }

    pub(crate) fn phrase(p: &mut Parser) {
        let m = p.start();
        super::phrases::phrase(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }

    pub(crate) fn pat(p: &mut Parser) {
        let m = p.start();
        super::patterns::pat(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }
}

fn identifier(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    p.bump(IDENT);
    m.complete(p, SyntaxKind::IDENTIFIER);
}

const LIT_KINDS: &[SyntaxKind] = &[CHAR, STRING];
const LIT_SET: TokenSet = TokenSet::new(LIT_KINDS);
fn literal(p: &mut Parser) {
    assert!(p.at_one_of(LIT_SET));

    for lit in [CHAR, STRING] {
        if p.at(lit) {
            let m = p.start();
            p.bump(lit);
            m.complete(p, SyntaxKind::LITERAL);
            return;
        }
    }
}

fn unit(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();

    p.bump(T!['(']);
    p.expect(T![')']);

    m.complete(p, SyntaxKind::UNIT);
}

fn meta_ident(p: &mut Parser) {
    assert!(p.at(T!['\'']));

    let m = p.start();

    p.bump(T!['\'']);
    p.expect(IDENT);

    m.complete(p, SyntaxKind::META_IDENT);
}
