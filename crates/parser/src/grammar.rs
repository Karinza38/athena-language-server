mod deductions;
mod directives;
mod expressions;
mod patterns;
mod phrases;
mod sorts;
mod statements;

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

    pub(crate) fn ded(p: &mut Parser) {
        let m = p.start();
        super::deductions::ded(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }

    pub(crate) fn dir(p: &mut Parser) {
        let m = p.start();
        super::directives::dir(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }

    pub(crate) fn stmt(p: &mut Parser) {
        let m = p.start();
        super::statements::stmt(p);
        if p.at(SyntaxKind::EOF) {
            m.abandon(p);
            return;
        }
        while !p.at(SyntaxKind::EOF) {
            p.bump_any();
        }
        m.complete(p, SyntaxKind::ERROR);
    }

    // test(file) source_file
    // module Foo {
    //    domain Bar
    //    declare Func: [Bar] -> Bar
    // }
    pub(crate) fn source_file(p: &mut Parser) {
        let m = p.start();

        while !p.at(SyntaxKind::EOF) {
            // test_err(file) source_error_with_extra_junk
            // module Foo {
            //    domain Bar ]][]
            //    declare Func: [Bar] -> Bar
            // }
            if !super::statements::stmt(p) || p.step_count() > 100 {
                // p.err_recover("invalid statement", STMT_START_SET);
                p.err_and_bump("invalid statement");
            }
            // test(file) source_file_with_semicolon
            // domain Bar;
            // domain Foo
            p.eat(T![;]);
        }

        m.complete(p, SyntaxKind::SOURCE_FILE);
    }
}

fn identifier(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    p.bump(IDENT);
    m.complete(p, SyntaxKind::IDENTIFIER);
}

fn op_annotated_param(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    identifier(p);

    p.expect(T![:]);

    p.expect(T!['(']);
    p.expect(T![OP]);

    if p.at(IDENT) {
        identifier(p);
    } else {
        p.error("expected operator arity");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::OP_ANNOTATED_PARAM);
}

fn typed_param(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    identifier(p);

    p.expect(T![:]);
    if !sorts::sort(p) {
        p.error("expected sort annotation");
    }

    m.complete(p, SyntaxKind::TYPED_PARAM);
}

fn maybe_typed_param(p: &mut Parser) {
    assert!(p.at(IDENT));
    if p.peek_at(T![:]) {
        if p.nth_at(2, T!['(']) && p.nth_at(3, T![OP]) {
            op_annotated_param(p);
        } else {
            typed_param(p);
        }
    } else {
        identifier(p);
    }
}

fn maybe_wildcard_typed_param(p: &mut Parser) -> bool {
    if p.at(T![_]) {
        let m = p.start();
        p.bump(T![_]);
        m.complete(p, SyntaxKind::WILDCARD);
    } else if p.at(IDENT) {
        maybe_typed_param(p);
    } else {
        return false;
    }

    true
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

fn prefix_binding(p: &mut Parser) -> bool {
    let m = p.start();
    if !p.at(T!['(']) {
        p.error("Expected to find a prefix binding in parens");
        m.abandon(p);
        return false;
    }
    p.bump(T!['(']);
    if !patterns::pat(p) {
        // test_err(expr) prefix_binding_no_pat
        // (let ( 1) foo)
        p.error("Expected to find a pattern for the binding");
    }

    if !phrases::phrase(p) {
        // test_err(expr) prefix_binding_no_phrase
        // (let (foo ) foo)
        p.error("Expected to find a value (phrase) for the binding");
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::PREFIX_BINDING);
    true
}
