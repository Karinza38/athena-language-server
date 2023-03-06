use crate::grammar::identifier;
use crate::grammar::patterns::pat;
use crate::grammar::phrases::phrase;
use crate::grammar::sorts::{sort_decl, SORT_DECL_START};
use crate::grammar::statements::{stmt, STMT_START_SET};
use crate::parser::Parser;
use crate::token_set::TokenSet;
use crate::{
    SyntaxKind::{self, IDENT},
    T,
};

fn module_dir(p: &mut Parser) {
    assert!(p.at(T![module]));

    let m = p.start();
    p.bump(T![module]);

    if !p.at(IDENT) {
        p.error("expected module name");
    } else {
        identifier(p);
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(dir) module_empty
        // module foo { }
        p.error("expected module body");
    }

    // test_err(dir) module_no_rbrace
    // module foo {
    while !p.at(T!['}']) && !p.at_end() {
        if !stmt(p) {
            p.err_recover(
                "unexpected input, expected statement in module body",
                STMT_START_SET,
            );
        }
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::MODULE_DIR);
}

// test(dir) domain_directive
// domain Bar

// test(dir) domain_complex_sort
// domain (List T)
fn domain_dir(p: &mut Parser) {
    assert!(p.at(T![domain]));

    let m = p.start();
    p.bump(T![domain]);

    if !sort_decl(p) {
        // test_err(dir) domain_empty
        // domain
        p.error("expected domain sort declaration");
    }

    m.complete(p, SyntaxKind::DOMAIN_DIR);
}

// test(dir) domains_directive
// domains Bar, Baz

// test(dir) domains_complex_sort
// domains (List T), (Set U)
fn domains_dir(p: &mut Parser) {
    assert!(p.at(T![domains]));

    let m = p.start();
    p.bump(T![domains]);

    if !sort_decl(p) {
        // test_err(dir) domains_empty
        // domains
        p.error("expected at least one sort declaration in domains directive");
    }

    while p.at(T![,]) || p.at_one_of(SORT_DECL_START) {
        p.expect(T![,]);
        if !sort_decl(p) {
            p.error("expected a sort declaration");
        }
    }

    m.complete(p, SyntaxKind::DOMAINS_DIR);
}

fn define_dir(p: &mut Parser) {
    assert!(p.at(T![define]));

    let m = p.start();
    p.bump(T![define]);

    if !p.at(IDENT) {
        // test_err(dir) define_no_name
        // define := true
        p.error("expected definition name");
    } else {
        identifier(p);
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_empty
        // define foo :=
        p.error("expected definition value");
    }

    m.complete(p, SyntaxKind::DEFINE_DIR);
}

// test(dir) define_proc
// define (foo a b) := lambda () b
fn define_proc_dir(p: &mut Parser) {
    assert!(p.at(T![define]) && p.peek_at(T!['(']));

    let m = p.start();

    p.bump(T![define]);

    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(dir) define_proc_no_name
        // define ( ) := lambda () true
        p.error("expected procedure name");
    } else {
        identifier(p);
    }

    // test_err(dir) define_proc_no_rparen
    // define (foo := lambda () true

    while !p.at(T![')']) && !p.at(T![:=]) {
        if p.at(IDENT) {
            identifier(p);
        } else {
            p.error("expected argument name");
        }
    }

    p.expect(T![')']);

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_proc_empty
        // define (foo) :=
        p.error("expected procedure body");
    }

    m.complete(p, SyntaxKind::DEFINE_PROC_DIR);
}

// test(dir) define_multi
// define [foo bar baz] := [true false true]
fn define_multi(p: &mut Parser) {
    assert!(p.at(T![define]) && p.peek_at(T!['[']));

    let m = p.start();
    p.bump(T![define]);

    p.bump(T!['[']);

    while !p.at(T![']']) && pat(p) {}

    p.expect(T![']']);

    p.expect(T![:=]);

    p.expect(T!['[']);

    while !p.at(T![']']) && phrase(p) {}

    p.expect(T![']']);

    m.complete(p, SyntaxKind::DEFINE_MULTI_DIR);
}

fn func_sorts(p: &mut Parser) {
    assert!(p.at(T!['[']));

    let m = p.start();
    p.bump(T!['[']);

    while !p.at(T![']']) && !p.at_end() {
        if !sort_decl(p) {
            p.error("expected a sort declaration");
        }
    }

    p.expect(T![']']);

    m.complete(p, SyntaxKind::FUNC_SORTS);
}

fn sort_vars_decl(p: &mut Parser) {
    assert!(p.at(T!['(']));

    let m = p.start();
    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(dir) sort_vars_no_name
        // declare foo : ( ) [] -> Int
        p.error("expected sort variable name");
    } else {
        identifier(p);
    }

    while p.at(T![,]) {
        p.bump(T![,]);
        super::sorts::ident_sort(p);
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::SORT_VARS_DECL);
}

// test(dir) declare_directive
// declare foo : [Int] -> Int
fn declare_dir(p: &mut Parser) {
    assert!(p.at(T![declare]));

    let m = p.start();
    p.bump(T![declare]);

    if !p.at(IDENT) {
        // test_err(dir) declare_no_name
        // declare : [Int] -> Int
        p.error("expected function symbol name");
    } else {
        identifier(p);
    }

    while p.at(T![,]) {
        p.bump(T![,]);
        if !p.at(IDENT) {
            // test_err(dir) declare_no_second_name
            // declare foo, : [Int] -> Int
            p.error("expected function symbol name, or trailing commas are not permitted");
        } else {
            identifier(p);
        }
    }

    p.expect(T![:]);

    if p.at(T!['(']) {
        // test(dir) declare_sort_vars
        // declare foo : (A) [A] -> (List A)
        sort_vars_decl(p);
    }

    if p.at(T!['[']) {
        func_sorts(p);
    } else {
        // test_err(dir) declare_no_sorts
        // declare foo : -> Int
        p.error("expected function argument sorts");
    }

    p.expect(T![->]);

    if !sort_decl(p) {
        // test_err(dir) declare_no_ret_sort
        // declare foo : [Int] ->
        p.error("expected function return sort");
    }

    m.complete(p, SyntaxKind::DECLARE_DIR);
}

// test(dir) load_dir
// load "list.ath"
fn load_dir(p: &mut Parser) {
    assert!(p.at(T![load]));

    let m = p.start();
    p.bump(T![load]);

    if !p.at(SyntaxKind::STRING) {
        // test_err(dir) load_no_string
        // load

        // test_err(dir) load_something_else
        // load 123
        p.error("expected string literal");
        if !p.at_one_of(STMT_START_SET) {
            p.bump_any();
        }
    } else {
        p.bump(SyntaxKind::STRING);
    }

    m.complete(p, SyntaxKind::LOAD_DIR);
}

pub(crate) const DIR_START_SET: TokenSet =
    TokenSet::new(&[T![module], T![domain], T![domains], T![define], T![declare]]);

pub(crate) fn dir(p: &mut Parser) -> bool {
    if p.at(T![module]) {
        module_dir(p);
    } else if p.at(T![domain]) {
        domain_dir(p);
    } else if p.at(T![domains]) {
        domains_dir(p);
    } else if p.at(T![define]) {
        if p.peek_at(T!['(']) {
            define_proc_dir(p);
        } else if p.peek_at(T!['[']) {
            define_multi(p);
        } else {
            define_dir(p);
        }
    } else if p.at(T![declare]) {
        declare_dir(p);
    } else if p.at(T![load]) {
        load_dir(p);
    } else {
        return false;
    }

    true
}
