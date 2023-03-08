use crate::grammar::expressions::expr;
use crate::grammar::identifier;
use crate::grammar::patterns::pat;
use crate::grammar::phrases::phrase;
use crate::grammar::sorts::sort_decl;
use crate::grammar::statements::{stmt, STMT_START_SET};
use crate::parser::Parser;
use crate::token_set::TokenSet;
use crate::{
    SyntaxKind::{self, IDENT},
    T,
};

// test(dir) module_directive
// module foo { declare joe: Person }
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

// test(dir) extend_module_directive
// extend-module Bar { declare joe: Person }
fn extend_module_dir(p: &mut Parser) {
    assert!(p.at(T![extend - module]));

    let m = p.start();
    p.bump(T![extend - module]);

    if !p.at(IDENT) {
        p.error("expected module name");
    } else {
        identifier(p);
    }

    p.expect(T!['{']);

    if p.at(T!['}']) {
        // test_err(dir) extend_module_empty
        // extend-module foo { }
        p.error("expected module body");
    }

    // test_err(dir) extend_module_no_rbrace
    // extend-module foo {
    while !p.at(T!['}']) && !p.at_end() {
        if !stmt(p) {
            p.err_recover(
                "unexpected input, expected statement in module body",
                STMT_START_SET,
            );
        }
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::EXTEND_MODULE_DIR);
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

    while p.at(T![,]) {
        p.expect(T![,]);
        if !sort_decl(p) {
            p.error("expected a sort declaration");
        }
    }

    m.complete(p, SyntaxKind::DOMAINS_DIR);
}

const NAME_SET: TokenSet = TokenSet::new(&[T![as], T![bind]]);
fn define_named_pattern(p: &mut Parser) {
    assert!(p.at(T!['(']) && (p.peek_at_one_of(NAME_SET) || p.nth_at_one_of(2, NAME_SET)));

    let m = p.start();
    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(dir) define_named_no_name
        // define ( as [a]) := true)
        p.error("expected name for define");
    } else {
        identifier(p);
    }

    p.bump_one_of(NAME_SET);

    if p.expect(T!['[']) {
        while !p.at(T![']']) && !p.at_end() {
            if !pat(p) {
                p.err_and_bump("expected pattern for define");
            }
        }
    }
    p.expect(T![']']);

    p.expect(T![')']);

    m.complete(p, SyntaxKind::DEFINE_NAMED_PATTERN);
}

// test(dir) define_dir
// define foo := true

// test(dir) define_private
// private define bar := false
fn define_dir(p: &mut Parser) {
    assert!((p.at(T![private]) && p.peek_at(T![define])) || p.at(T![define]));

    let m = p.start();
    p.eat(T![private]);
    p.bump(T![define]);

    if p.at(T!['(']) {
        define_named_pattern(p);
    } else {
        if !p.at(IDENT) {
            // test_err(dir) define_no_name
            // define := true
            p.error("expected definition name");
        } else {
            identifier(p);
        }
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

// test(dir) define_proc_private
// private define (foo b) := b
fn define_proc_dir(p: &mut Parser) {
    assert!((p.at(T![private]) && p.peek_at(T![define])) || p.at(T![define]) && p.peek_at(T!['(']));

    let m = p.start();
    p.eat(T![private]);
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
        if p.at(IDENT) || p.at(T![_]) {
            super::maybe_wildcard_typed_param(p);
        } else {
            p.err_and_bump("expected parameter");
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

// test(dir) define_multi_private
// private define [a b] := [1 2]
fn define_multi(p: &mut Parser) {
    assert!((p.at(T![private]) && p.peek_at(T![define])) || p.at(T![define]) && p.peek_at(T!['[']));

    let m = p.start();
    p.eat(T![private]);
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

fn input_transform_decl(p: &mut Parser) {
    assert!(p.at(T!['[']));

    let m = p.start();
    p.bump(T!['[']);

    while !p.at(T![']']) && !p.at_end() {
        if !expr(p) {
            p.err_and_bump("expected an expression in input transform");
        }
    }

    p.expect(T![']']);
    m.complete(p, SyntaxKind::INPUT_TRANSFORM_DECL);
}

fn declare_attr(p: &mut Parser) {
    let m = p.start();
    if p.at(T![left - assoc]) {
        p.bump(T![left - assoc]);
    } else if p.at(T![right - assoc]) {
        p.bump(T![right - assoc]);
    } else if p.at(IDENT) {
        identifier(p);
    } else {
        p.err_and_bump("expected a declaration attribute (left-assoc, right-assoc, or identifier)");
    }
    m.complete(p, SyntaxKind::DECLARE_ATTR);
}

// test(dir) declare_attrs
// declare foo : [Int] -> Int [left-assoc]
fn declare_attrs(p: &mut Parser) {
    assert!(p.at(T!['[']));

    let m = p.start();
    p.bump(T!['[']);

    if p.at(T![']']) {
        // test_err(dir) declare_attrs_empty
        // declare foo : [Int] -> Int []
        p.error("expected at least one attribute in declaration attributes");
    }

    while !p.at(T![']']) && !p.at(T!['[']) && !p.at_end() {
        // test(dir) declare_attrs_multiple
        // declare foo : [Int] -> Int [100 bar left-assoc]
        declare_attr(p);
    }

    if p.at(T!['[']) {
        // test(dir) declare_attrs_with_transform
        // declare foo : [Int] -> Int [100 [int->nat]]
        input_transform_decl(p);
    }
    p.expect(T![']']);

    m.complete(p, SyntaxKind::DECLARE_ATTRS);
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

    // test(dir) declare_no_args
    // declare foo : Int
    if p.at(T!['[']) {
        func_sorts(p);
        p.expect(T![->]);
    }

    if !sort_decl(p) {
        // test_err(dir) declare_no_ret_sort
        // declare foo : [Int] ->
        p.error("expected function return sort");
    }

    if p.at(T!['[']) {
        // test(dir) declare_with_attrs
        // declare foo : [Int] -> Int [100 left-assoc bar [int->nat]]
        declare_attrs(p);
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

// test(dir) assert_dir
// assert (foo = foo)
fn assert_dir(p: &mut Parser) {
    assert!(p.at(T![assert]));

    let m = p.start();
    p.bump(T![assert]);

    if p.at(IDENT) && p.peek_at(T![:=]) || p.at(T![:=]) {
        // test(dir) assert_named
        // assert foo := (bar = bar)

        if !p.at(IDENT) {
            // test_err(dir) assert_no_name
            // assert := true
            p.error("expected identifier for the assertion");
        } else {
            identifier(p);
        }
        p.expect(T![:=]);
    }

    if !expr(p) {
        // test_err(dir) assert_empty
        // assert
        p.error("expected assertion");
    }

    m.complete(p, SyntaxKind::ASSERT_DIR);
}

// test(dir) assert_closed_dir
// assert* (foo = foo)
fn assert_closed_dir(p: &mut Parser) {
    assert!(p.at(T![assert*]));

    let m = p.start();
    p.bump(T![assert*]);

    if p.at(IDENT) && p.peek_at(T![:=]) || p.at(T![:=]) {
        // test(dir) assert_closed_named
        // assert* foo := (bar = bar)

        if !p.at(IDENT) {
            // test_err(dir) assert_closed_no_name
            // assert* := true
            p.error("expected identifier for the closed assertion");
        } else {
            identifier(p);
        }
        p.expect(T![:=]);
    }

    if !expr(p) {
        // test_err(dir) assert_closed_empty
        // assert*
        p.error("expected assertion");
    }

    m.complete(p, SyntaxKind::ASSERT_CLOSED_DIR);
}

pub(crate) const DIR_START_SET: TokenSet = TokenSet::new(&[
    T![module],
    T![extend - module],
    T![domain],
    T![domains],
    T![define],
    T![declare],
    T![load],
    T![assert],
    T![assert*],
    T![private],
]);

pub(crate) fn dir(p: &mut Parser) -> bool {
    #[cfg(test)]
    eprintln!("dir: {:?} {:?}", p.current(), p.nth(1));

    match p.current() {
        T![module] => {
            module_dir(p);
        }
        T![extend - module] => {
            extend_module_dir(p);
        }
        T![domain] => {
            domain_dir(p);
        }
        T![domains] => {
            domains_dir(p);
        }
        T![private] => {
            if p.peek_at(T![define]) {
                if p.nth_at(2, T!['(']) {
                    define_proc_dir(p);
                } else if p.nth_at(2, T!['[']) {
                    define_multi(p);
                } else {
                    define_dir(p);
                }
            } else {
                p.error("private must be followed by define");
                return false;
            }
        }
        T![define] => {
            if p.peek_at(T!['(']) {
                let three_la = p.nth(3);

                if p.nth_at_one_of(2, NAME_SET) || NAME_SET.contains(three_la) {
                    define_dir(p);
                } else {
                    define_proc_dir(p);
                }
            } else if p.peek_at(T!['[']) {
                define_multi(p);
            } else {
                define_dir(p);
            }
        }
        T![declare] => {
            declare_dir(p);
        }
        T![load] => {
            load_dir(p);
        }
        T![assert] => {
            assert_dir(p);
        }
        T![assert*] => {
            assert_closed_dir(p);
        }
        _ => {
            return false;
        }
    }

    true
}
