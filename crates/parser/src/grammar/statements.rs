use crate::{parser::Parser, SyntaxKind};

use super::directives::DIR_START_SET;

// test(stmt) phrase_stmt
// (!claim A)
fn phrase_stmt(p: &mut Parser) {
    // test_err(stmt) phrase_stmt_err
    // (! )
    let m = p.start();
    super::phrases::phrase(p);
    m.complete(p, SyntaxKind::PHRASE_STMT);
}

// test(stmt) dir_stmt
// domain Foo
fn dir_stmt(p: &mut Parser) {
    // test_err(stmt) dir_stmt_err
    // domain
    let m = p.start();
    super::directives::dir(p);
    m.complete(p, SyntaxKind::DIR_STMT);
}

pub(crate) fn stmt(p: &mut Parser) {
    if p.at_one_of(DIR_START_SET) {
        dir_stmt(p);
    } else {
        phrase_stmt(p);
    }
}
