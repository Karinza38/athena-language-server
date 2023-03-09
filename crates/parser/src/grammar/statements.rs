use crate::{
    grammar::{directives::DIR_AFTER_LPAREN, identifier, sorts::SORT_DECL_START},
    parser::Parser,
    token_set::TokenSet,
    SyntaxKind::{self, IDENT},
    T,
};

use super::{directives::DIR_START_SET, phrases::PHRASE_START_SET, sorts::sort_decl};

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

fn structure_name_def(p: &mut Parser) {
    assert!(p.at(IDENT) || p.at(T!['(']));

    let m = p.start();
    if !sort_decl(p) {
        p.err_recover(
            "expected a structure name definition",
            TokenSet::new(&[T![:=]]),
        );
    }
    m.complete(p, SyntaxKind::STRUCTURE_NAME_DEF);
}

fn constant_constructor(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    identifier(p);
    m.complete(p, SyntaxKind::CONSTANT_CONSTRUCTOR);
}

fn compound_constructor(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);
    if !p.at(IDENT) {
        p.error("expected an identifier for the constructor");
    } else {
        identifier(p);
    }
    while !p.at(T![')']) && !p.at_end() {
        maybe_tagged_sort_decl(p);
    }

    p.expect(T![')']);
    m.complete(p, SyntaxKind::COMPOUND_CONSTRUCTOR);
}

fn structure_constructor(p: &mut Parser) {
    assert!(p.at_one_of(SORT_DECL_START));

    if p.at(IDENT) {
        constant_constructor(p);
    } else {
        compound_constructor(p);
    }
}

fn maybe_tagged_sort_decl(p: &mut Parser) {
    assert!(p.at_one_of(SORT_DECL_START));

    let m = p.start();
    if p.peek_at(T![:]) {
        if !p.at(IDENT) {
            p.error("expected an identifier for the tag");
        } else {
            // tag
            identifier(p);
        }
        p.bump(T![:]);
    }
    if !sort_decl(p) {
        p.err_recover("expected a sort declaration", TokenSet::new(&[T![')']]));
    }
    m.complete(p, SyntaxKind::MAYBE_TAGGED_SORT_DECL);
}

fn structure_def(p: &mut Parser) {
    assert!(p.at_one_of(SORT_DECL_START));

    let m = p.start();
    structure_name_def(p);
    p.expect(T![:=]);

    if p.at_contextual_kw(T![|]) || p.at(T![|]) {
        // test_err(stmt) structure_def_leading_pipe
        // datatype Foo := | bar
        p.error("leading `|` is not allowed in constructor list");
    } else if p.at_one_of(SORT_DECL_START) {
        structure_constructor(p);
    } else {
        // test_err(stmt) structure_def_no_constructor
        // datatype Foo :=
        p.error("expected at least one constructor");
    }

    while p.at_contextual_kw(T![|]) {
        // test(stmt) structure_def_multiple_constructors
        // datatype Foo := bar | baz | (quz num:Int)
        p.bump_remap(T![|]);
        if p.at_one_of(SORT_DECL_START) {
            structure_constructor(p);
        } else {
            // test_err(stmt) structure_def_trailing_pipe
            // datatype Foo := bar |
            p.error("expected a constructor following the `|`");
        }
    }

    m.complete(p, SyntaxKind::STRUCTURE_DEF);
}

// test(stmt) datatype_stmt
// datatype Foo := bar | (baz num:Int)
fn datatype_stmt(p: &mut Parser) {
    assert!(p.at(T![datatype]));

    let m = p.start();
    p.bump(T![datatype]);
    if p.at_one_of(SORT_DECL_START) {
        structure_def(p);
    } else {
        // test_err(stmt) datatype_stmt_err
        // datatype
        p.error("expected a datatype definition");
    }
    m.complete(p, SyntaxKind::DATATYPE_STMT);
}

// test(stmt) datatypes_stmt
// datatypes Foo := bar | (baz num:Int) && Baz := quz | (qux val:Int)
fn datatypes_stmt(p: &mut Parser) {
    assert!(p.at(T![datatypes]));

    let m = p.start();
    p.bump(T![datatypes]);
    if p.at_one_of(SORT_DECL_START) {
        structure_def(p);
    } else {
        // test_err(stmt) datatypes_stmt_err
        // datatypes
        p.error("expected a datatype definition");
    }

    while p.at(T![&&]) {
        // test(stmt) datatypes_stmt_multiple_structures
        // datatypes Foo := bar && Baz := quz
        p.bump_remap(T![&&]);
        if p.at_one_of(SORT_DECL_START) {
            structure_def(p);
        } else {
            // test_err(stmt) datatypes_stmt_trailing_and
            // datatypes Foo := bar &&
            p.error("expected a datatype definition following the `&&`");
        }
    }

    m.complete(p, SyntaxKind::DATATYPES_STMT);
}

// test(stmt) structure_stmt
// structure Foo := bar | (baz num:Int)
fn structure_stmt(p: &mut Parser) {
    assert!(p.at(T![structure]));

    let m = p.start();
    p.bump(T![structure]);
    if p.at_one_of(SORT_DECL_START) {
        structure_def(p);
    } else {
        // test_err(stmt) structure_stmt_err
        // structure
        p.error("expected a structure definition");
    }
    m.complete(p, SyntaxKind::STRUCTURE_STMT);
}

// test(stmt) structures_stmt
// structures Foo := bar && Baz := quz
fn structures_stmt(p: &mut Parser) {
    assert!(p.at(T![structures]));

    let m = p.start();
    p.bump(T![structures]);
    if p.at_one_of(SORT_DECL_START) {
        structure_def(p);
    } else {
        // test_err(stmt) structures_stmt_err
        // structures
        p.error("expected a structure definition");
    }

    while p.at(T![&&]) {
        // test(stmt) structures_stmt_multiple_structures
        // structures Foo := bar && Baz := quz
        p.bump_remap(T![&&]);
        if p.at_one_of(SORT_DECL_START) {
            structure_def(p);
        } else {
            // test_err(stmt) structures_stmt_trailing_and
            // structures Foo := bar &&
            p.error("expected a structure definition following the `&&`");
        }
    }
    m.complete(p, SyntaxKind::STRUCTURES_STMT);
}

pub(crate) const STMT_START_SET: TokenSet =
    DIR_START_SET.union(PHRASE_START_SET).union(TokenSet::new(&[
        T![structure],
        T![structures],
        T![datatype],
        T![datatypes],
    ]));

pub(crate) fn stmt(p: &mut Parser) -> bool {
    #[cfg(test)]
    eprintln!("stmt: {:?} {:?} {:?}", p.current(), p.nth(1), p.nth(2));

    if p.at_one_of(DIR_START_SET.subtract(PHRASE_START_SET)) {
        dir_stmt(p);
    } else if p.at_one_of(PHRASE_START_SET.subtract(DIR_START_SET)) {
        phrase_stmt(p);
    } else if p.at_one_of(PHRASE_START_SET.intersect(DIR_START_SET)) {
        assert!(p.at(T!['(']));

        if p.peek_at_one_of(DIR_AFTER_LPAREN) {
            dir_stmt(p);
        } else {
            phrase_stmt(p);
        }
    } else {
        match p.current() {
            T![structure] => structure_stmt(p),
            T![structures] => structures_stmt(p),
            T![datatype] => datatype_stmt(p),
            T![datatypes] => datatypes_stmt(p),
            _ => {
                return false;
            }
        }
    }

    true
}
