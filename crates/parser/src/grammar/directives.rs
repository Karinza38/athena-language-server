use crate::grammar::expressions::expr;
use crate::grammar::phrases::phrase;
use crate::grammar::sorts::{sort_decl, SORT_DECL_START};
use crate::grammar::statements::{stmt, STMT_START_SET};
use crate::grammar::{identifier, maybe_wildcard_typed_param};
use crate::parser::{Marker, Parser};
use crate::token_set::TokenSet;
use crate::{
    SyntaxKind::{self, IDENT},
    T,
};

// test(dir) module_directive
// module foo { declare joe: Person }
fn module_dir(p: &mut Parser) {
    assert!(p.at(T![module]) || p.at_prefix_kw(T![module]));

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
        p.eat(T![;]);
    }

    p.expect(T!['}']);
    m.complete(p, SyntaxKind::INFIX_MODULE_DIR);
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
        p.eat(T![;]);
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

// test(dir) define_named
// define (foo as [a]) := true
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

    if p.at(T!['[']) {
        super::patterns::list_pat(p);
    } else {
        p.error("expected list pattern for named define");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::DEFINE_NAMED_PATTERN);
}

// test(dir) define_proc
// define (foo a b) := lambda () b

// test(dir) define_proc_private
// private define (foo b) := b
fn define_proc(p: &mut Parser) {
    let m = p.start();

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

    m.complete(p, SyntaxKind::DEFINE_PROC);
}

enum DefineName {
    Proc,
    Other,
}

fn define_name(p: &mut Parser) -> DefineName {
    match p.current() {
        T!['('] => {
            if p.peek_at_one_of(NAME_SET) || p.nth_at_one_of(2, NAME_SET) {
                define_named_pattern(p);
                DefineName::Other
            } else {
                define_proc(p);
                DefineName::Proc
            }
        }
        T!['['] => {
            // test(dir) define_multi
            // define [foo bar baz] := [true false true]

            // test(dir) define_multi_private
            // private define [a b] := [1 2]

            // test(dir) define_multi_one
            // define [foo] := true
            super::patterns::list_pat(p);
            DefineName::Other
        }
        IDENT => {
            identifier(p);
            DefineName::Other
        }
        _ => {
            // test_err(dir) define_no_name
            // define := true
            p.error("expected definition name");
            DefineName::Other
        }
    }
}

// test(dir) define_dir
// define foo := true
fn define_dir(p: &mut Parser) {
    assert!((p.at(T![private]) && p.peek_at(T![define])) || p.at(T![define]));

    let m = p.start();

    // test(dir) define_private
    // private define bar := false
    p.eat(T![private]);

    p.bump(T![define]);

    define_name(p);

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_empty
        // define foo :=
        p.error("expected definition value");
    }

    m.complete(p, SyntaxKind::INFIX_DEFINE_DIR);
}

enum DefBlockParseState {
    Start,
    AteDefineProc(Marker),
}

fn def_block(p: &mut Parser, state: DefBlockParseState) {
    fn after_proc(p: &mut Parser, m: Marker) {
        p.eat(T![:=]);

        if !phrase(p) {
            // test_err(dir) define_block_empty
            // (define (foo a b) := )
            p.error("expected definition value");
        }
        m.complete(p, SyntaxKind::PREFIX_DEFINE_BLOCK);
    }

    match state {
        DefBlockParseState::Start => {
            assert!(p.at(T!['(']));

            let m = p.start();
            define_proc(p);

            after_proc(p, m);
        }
        DefBlockParseState::AteDefineProc(m) => after_proc(p, m),
    }
}

// test(dir) prefix_define_blocks
// (define (foo a b) := a (foo b c) := b)
fn prefix_define_blocks(p: &mut Parser, define: Marker) {
    // the parse state is after the '(' and 'define' has been eaten
    // and potentially the first def block
    while !p.at(T![')']) && !p.at_end() {
        if p.at(T!['(']) {
            def_block(p, DefBlockParseState::Start);
        } else {
            // test_err(dir) define_prefix_no_block
            // (define (foo a b) a b)
            p.err_recover("expected define block", TokenSet::new(&[T![')']]));
        }
    }

    p.expect(T![')']);

    define.complete(p, SyntaxKind::PREFIX_DEFINE_BLOCKS);
}

// test(dir) define_prefix
// (define A B)
fn prefix_define(p: &mut Parser) {
    assert!(p.at(T!['(']) && p.peek_at(T![define]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![define]);

    // test_err(dir) define_prefix_no_name
    // (define)

    // test(dir) define_prefix_proc
    // (define (foo a b) (lambda () b))
    let maybe_def_block = p.start();
    match define_name(p) {
        DefineName::Proc => {
            def_block(p, DefBlockParseState::AteDefineProc(maybe_def_block));
            prefix_define_blocks(p, m);
        }
        DefineName::Other => {
            maybe_def_block.abandon(p); // def blocks are always procedure defs
            if !phrase(p) {
                // test_err(dir) define_prefix_no_value
                // (define foo)
                p.error("expected value for define");
            }
            p.expect(T![')']);
            m.complete(p, SyntaxKind::PREFIX_DEFINE);
        }
    }
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

/// Parses a sort variable declaration (e.g. `(A,B,C)` in `declare foo: (A,B,C) -> Int`)
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

enum SortVarsOrConstantSort {
    SortVars,
    ConstantSort,
    Ambig,
}

fn sort_vars_or_constant_sort(p: &mut Parser) -> SortVarsOrConstantSort {
    assert!(p.at(T!['(']));

    let peek1 = p.nth(1);
    let peek2 = p.nth(2);

    match (peek1, peek2) {
        (T![')'], _) => {
            let m = p.start();
            p.bump(T!['(']);
            p.bump(T![')']);

            if p.at(T!['[']) {
                p.error("Must have at least one sort variable in sort variable declaration");
                m.complete(p, SyntaxKind::SORT_VARS_DECL);
                return SortVarsOrConstantSort::SortVars;
            } else {
                m.complete(p, SyntaxKind::COMPOUND_SORT_DECL);
                return SortVarsOrConstantSort::ConstantSort;
            }
        }
        (IDENT, T![,]) | (IDENT, T![')']) => {
            sort_vars_decl(p);
            return SortVarsOrConstantSort::SortVars;
        }
        (IDENT, IDENT) | (IDENT, T!['(']) => {
            if !sort_decl(p) {
                p.error("expected a sort declaration");
            }
            return SortVarsOrConstantSort::ConstantSort;
        }
        _ => {
            p.err_recover(
                "expected a sort variable declaration or a constant sort",
                TokenSet::new(&[T![')'], T!['['], T![->]]).union(SORT_DECL_START), // FIXME: unsure about this recovery set
            );
            if p.at(T!['[']) {
                return SortVarsOrConstantSort::SortVars;
            } else {
                return SortVarsOrConstantSort::Ambig;
            }
        }
    }
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

        match sort_vars_or_constant_sort(p) {
            SortVarsOrConstantSort::SortVars => {}
            SortVarsOrConstantSort::ConstantSort => {
                // test(dir) declare_constant_compound_sort
                // declare foo : (List A)
                m.complete(p, SyntaxKind::CONSTANT_DECLARE_DIR);
                return;
            }
            SortVarsOrConstantSort::Ambig => {
                p.error("ambiguous declare directive");
            }
        }
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

enum LoadKind {
    Prefix,
    Infix,
}

// test(dir) load_dir
// load "list.ath"
fn load_dir(p: &mut Parser, kind: LoadKind) {
    let prefix = matches!(kind, LoadKind::Prefix);
    assert!((prefix && p.at_prefix_kw(T![load])) || (!prefix && p.at(T![load])));

    let m = p.start();

    if prefix {
        // test(dir) load_prefix
        // (load "list.ath")
        p.bump(T!['(']);
    }

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

    if prefix {
        p.expect(T![')']);
    }

    m.complete(p, SyntaxKind::LOAD_DIR);
}

// test(dir) prefix_assert
// (assert (bar = bar) (foo = foo))
fn prefix_assert_dir(p: &mut Parser) {
    assert!(p.at_prefix_kw(T![assert]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![assert]);

    if p.at(IDENT) && p.peek_at(T![:=]) || p.at(T![:=]) {
        // test(dir) prefix_assert_named
        // (assert foo := (bar = bar))

        if !p.at(IDENT) {
            // test_err(dir) prefix_assert_no_name
            // (assert := true)
            p.error("expected identifier for the assertion");
        } else {
            identifier(p);
        }
        p.bump(T![:=]);
    }

    if !expr(p) {
        // test_err(dir) prefix_assert_empty
        // (assert foo := )
        p.error("expected at least one assertion");
    }

    while !p.at(T![')']) && !p.at_end() {
        if !expr(p) {
            p.err_recover("expected assertion", TokenSet::single(T![')']));
        }
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::PREFIX_ASSERT_DIR);
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

    while p.at(T![,]) {
        // test(dir) assert_multiple
        // assert A, B, C
        p.bump(T![,]);
        if !expr(p) {
            // test_err(dir) assert_multiple_empty
            // assert A,
            p.error("expected assertion after comma");
        }
    }

    m.complete(p, SyntaxKind::INFIX_ASSERT_DIR);
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

// test(dir) open_dir
// open List, Set
fn open_dir(p: &mut Parser) {
    assert!(p.at(T![open]));

    let m = p.start();
    p.bump(T![open]);

    if !p.at(IDENT) {
        // test_err(dir) open_no_name
        // open
        p.error("expected module name");
    } else {
        identifier(p);
    }

    while p.at(T![,]) {
        p.bump(T![,]);
        if !p.at(IDENT) {
            // test_err(dir) open_no_second_name
            // open List,
            p.error("expected module name, or trailing commas are not permitted");
        } else {
            identifier(p);
        }
    }

    m.complete(p, SyntaxKind::OPEN_DIR);
}

// test(dir) associativity_dir_left
// left-assoc Foo

// test(dir) associativity_dir_right
// right-assoc Foo
fn associativity_dir(p: &mut Parser) {
    assert!(p.at_one_of(ASSOCIATIVITY_SET));

    let m = p.start();
    p.bump_one_of(ASSOCIATIVITY_SET);

    if !p.at(IDENT) {
        // test_err(dir) associativity_dir_no_identifier
        // left-assoc
        p.error("expected identifier to set associativity for");
    } else {
        identifier(p);
    }

    m.complete(p, SyntaxKind::ASSOCIATIVITY_DIR);
}

fn parameter_list_ident_start(p: &mut Parser) {
    assert!(p.at(T!['(']));

    p.bump(T!['(']);

    if !p.at(IDENT) {
        // test_err(dir) parameter_list_ident_start_no_ident
        // primitive-method ( ) := foo
        p.error("expected identifier for primitive method");
    } else {
        identifier(p);
    }

    while !p.at(T![')']) && !p.at_end() {
        if !maybe_wildcard_typed_param(p) {
            // test_err(dir) parameter_list_ident_start_no_param
            // primitive-method ( foo domain Foo) := foo
            p.err_recover(
                "expected parameter",
                TokenSet::new(&[IDENT, T![')'], T![:=]]),
            );
        }
    }

    p.expect(T![')']);
}

// test(dir) infix_rule_dir
// primitive-method (false-intro) := (not (false))
fn infix_rule_dir(p: &mut Parser) {
    assert!(p.at(T![primitive - method]));

    let m = p.start();
    p.bump(T![primitive - method]);

    parameter_list_ident_start(p);

    if !p.eat(T![:=]) {
        // test_err(dir) infix_rule_dir_no_assign
        // primitive-method (foo) foo
        p.error("expected `:=` after primitive method parameters");
    }

    if !expr(p) {
        // test_err(dir) infix_rule_dir_no_expr
        // primitive-method (foo) :=
        p.error("expected body for primitive method");
    }

    m.complete(p, SyntaxKind::INFIX_RULE_DIR);
}

// test(dir) prefix_rule_dir
// (primitive-method (false-intro)
//  (not (false)))
fn prefix_rule_dir(p: &mut Parser) {
    assert!(p.at(T!['(']) && p.peek_at(T![primitive - method]));

    let m = p.start();
    p.bump(T!['(']);
    p.bump(T![primitive - method]);

    parameter_list_ident_start(p);

    if !expr(p) {
        // test_err(dir) prefix_rule_dir_no_expr
        // (primitive-method (foo) )
        p.error("expected body for primitive method");
    }

    p.expect(T![')']);

    m.complete(p, SyntaxKind::PREFIX_RULE_DIR);
}

// test(dir) infix_expand_input_dir
// expand-input foo, bar, baz [nat->int]
fn expand_input_dir(p: &mut Parser) {
    assert!(p.at(T![expand - input]));

    let m = p.start();
    p.bump(T![expand - input]);

    if !phrase(p) {
        // test_err(dir) infix_expand_input_dir_no_phrase
        // expand-input
        p.error("expected phrase to expand input for");
    }

    while p.eat(T![,]) {
        if !phrase(p) {
            // test_err(dir) infix_expand_input_dir_no_second_phrase
            // expand-input foo,
            p.error("expected phrase after comma");
        }
    }

    if !phrase(p) {
        // test_err(dir) infix_expand_input_dir_no_third_phrase
        // expand-input foo, bar
        p.error("expected input expansion");
    }

    m.complete(p, SyntaxKind::EXPAND_INPUT_DIR);
}

// test(dir) define_sort_dir
// define-sort Foo := Int
fn define_sort_dir(p: &mut Parser) {
    assert!(p.at(T![define - sort]));

    // test(file) define_sort_dir_file
    // define-sort Foo := Int

    let m = p.start();
    p.bump(T![define - sort]);

    if !p.at(IDENT) {
        // test_err(dir) define_sort_dir_no_ident
        // define-sort
        p.error("expected identifier for sort definition");
    } else {
        identifier(p);
    }

    p.expect(T![:=]);

    if !phrase(p) {
        // test_err(dir) define_sort_dir_no_phrase
        // define-sort Foo :=
        p.error("expected phrase for sort definition");
    }

    m.complete(p, SyntaxKind::DEFINE_SORT_DIR);
}

const ASSOCIATIVITY_SET: TokenSet = TokenSet::new(&[T![left - assoc], T![right - assoc]]);

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
    T![open],
    T![left - assoc],
    T![right - assoc],
    T!['('],
    T![primitive - method],
    T![expand - input],
    T![define - sort],
]);

pub(crate) const DIR_AFTER_LPAREN: TokenSet = TokenSet::new(&[
    T![module],
    T![define],
    T![domain],
    T![domains],
    T![declare],
    T![load],
    T![assert],
    T![assert*],
    T![primitive - method],
    T![load],
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
                define_dir(p);
            } else {
                p.error("private must be followed by define");
                return false;
            }
        }
        T![define] => {
            define_dir(p);
        }
        T![declare] => {
            declare_dir(p);
        }
        T![load] => {
            load_dir(p, LoadKind::Infix);
        }
        T![assert] => {
            assert_dir(p);
        }
        T![assert*] => {
            assert_closed_dir(p);
        }
        T![open] => {
            open_dir(p);
        }
        T![left - assoc] | T![right - assoc] => {
            associativity_dir(p);
        }
        T![primitive - method] => {
            infix_rule_dir(p);
        }
        T![expand - input] => {
            expand_input_dir(p);
        }
        T![define - sort] => {
            define_sort_dir(p);
        }
        T!['('] => match p.nth(1) {
            T![primitive - method] => {
                prefix_rule_dir(p);
            }
            T![define] => {
                prefix_define(p);
            }
            T![load] => {
                load_dir(p, LoadKind::Prefix);
            }
            T![assert] => {
                prefix_assert_dir(p);
            }
            _ => {
                return false;
            }
        },
        _ => {
            return false;
        }
    }

    true
}
