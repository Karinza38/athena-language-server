use crate::{parser::Parser, SyntaxKind, T};

// test(expr) simple_ident_sort
// ?x: bar
pub(crate) fn ident_sort(p: &mut Parser) {
    assert!(p.at(SyntaxKind::IDENT));

    let m = p.start();
    super::identifier(p);
    m.complete(p, SyntaxKind::IDENT_SORT);
}

// test(expr) simple_var_sort
// ?x: 'foo
fn var_sort(p: &mut Parser) {
    assert!(p.at(T!['\'']));

    let m = p.start();
    p.bump(T!['\'']);
    if p.at(SyntaxKind::IDENT) {
        super::identifier(p);
    } else {
        p.error("Expected identifier as part of a sort variable");
    }
    m.complete(p, SyntaxKind::VAR_SORT);
}

// test(expr) simple_compound_sort
// ?x: (List Int)
fn compound_sort(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);
    let mut found_sort = sort(p);
    if !found_sort {
        p.error("Expected at least one sort in a compound sort");
    } else {
        while found_sort {
            found_sort = sort(p);
        }
    }
    p.expect(T![')']);
    m.complete(p, SyntaxKind::COMPOUND_SORT);
}

fn compound_sort_decl(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    if !sort_decl(p) {
        p.error("Expected at least one sort in a compound sort");
    }

    while !p.at(T![')']) {
        if !sort_decl(p) {
            p.error("Expected a sort in a compound sort");
            break;
        }
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::COMPOUND_SORT_DECL);
}

pub(crate) fn sort_decl(p: &mut Parser) -> bool {
    if p.at(SyntaxKind::IDENT) {
        ident_sort(p);
    } else if p.at(T!['(']) {
        compound_sort_decl(p);
    } else {
        return false;
    }
    true
}

// test(expr) simple_sort
// ?x: Int
pub(crate) fn sort(p: &mut Parser) -> bool {
    if p.at(SyntaxKind::IDENT) {
        ident_sort(p);
        true
    } else if p.at(T!['\'']) {
        var_sort(p);
        true
    } else if p.at(T!['(']) {
        compound_sort(p);
        true
    } else {
        false
    }
}
