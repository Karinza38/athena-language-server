mod sourcegen_inline_tests;

use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use insta::assert_snapshot;

use crate::{EntryPoint, LexedInput};

#[test]
fn lex_ok() {
    for case in TestCase::list("lexer/ok") {
        let actual = lex(&case.text);
        insta::with_settings!({description => &case.text, omit_expression => true }, { assert_snapshot!(actual) });
    }
}

#[test]
fn lex_err() {
    for case in TestCase::list("lexer/err") {
        let actual = lex(&case.text);
        assert_snapshot!(actual)
    }
}

fn lex(text: &str) -> String {
    let lexed = LexedInput::new(text);

    let mut res = String::new();
    for i in 0..lexed.len() {
        let kind = lexed.kind(i);
        let text = lexed.text(i);
        let error = lexed.error(i);

        let error = error
            .map(|err| format!(" error: {err}"))
            .unwrap_or_default();
        writeln!(res, "{kind:?} {text:?}{error}").unwrap();
    }
    res
}

macro_rules! test_glob {
    (ok $glob: expr) => {
        insta::glob!("../test_data", &format!("parser/{}/*.ath", $glob), |path| {
            ok_test(path);
        });
    };
    (err $glob: expr) => {
        insta::glob!("../test_data", &format!("parser/{}/*.ath", $glob), |path| {
            err_test(path);
        });
    };
}

fn ok_test(p: &Path) {
    let case = TestCase::read(p);
    eprintln!("running test: {}", case.ath.display());
    let (actual, errors) = parse(case.entry, &case.text);
    assert!(
        !errors,
        "errors in an OK file {}:\n{actual}",
        case.ath.display()
    );
    insta::
    
    with_settings!({description => &case.text, omit_expression => true }, { assert_snapshot!(actual) });
}

fn err_test(p: &Path) {
    let case = TestCase::read(p);
    eprintln!("running test: {}", case.ath.display());
    let (actual, errors) = parse(case.entry, &case.text);
    assert!(
        errors,
        "no errors in an ERR file {}:\n{actual}",
        case.ath.display()
    );
    insta::with_settings!({description => &case.text, omit_expression => true }, { assert_snapshot!(actual) });
}

#[test]
fn parse_ok() {
    for case in TestCase::list("parser/ok") {
        let (actual, errors) = parse(EntryPoint::SourceFile, &case.text);
        assert!(
            !errors,
            "errors in an OK file {}:\n{actual}",
            case.ath.display()
        );
        assert_snapshot!(actual)
    }
}

#[test]
fn parse_err() {
    for case in TestCase::list("parser/err") {
        let (actual, errors) = parse(EntryPoint::SourceFile, &case.text);
        assert!(
            errors,
            "no errors in an ERR file {}:\n{actual}",
            case.ath.display()
        );
        assert_snapshot!(actual)
    }
}

#[test]
fn parse_inline_ok() {
    test_glob!(ok "inline/ok/expr");
    test_glob!(ok "inline/ok/pat");
    test_glob!(ok "inline/ok/ded");
}

#[test]
fn parse_inline_err() {
    test_glob!(err "inline/err/expr");
    test_glob!(err "inline/err/pat");
    test_glob!(err "inline/err/ded");
}

fn parse(entry: EntryPoint, text: &str) -> (String, bool) {
    let lexed = LexedInput::new(text);
    let input = lexed.to_parser_input();
    let output = entry.parse(&input);

    let mut buf = String::new();
    let mut errors = Vec::new();
    let mut indent = String::new();
    let mut depth = 0;
    let mut len = 0;
    lexed.intersperse_trivia(&output, &mut |step| match step {
        crate::StrStep::Token { kind, text } => {
            assert!(depth > 0);
            len += text.len();
            writeln!(buf, "{indent}{kind:?} {text:?}").unwrap();
        }
        crate::StrStep::Enter { kind } => {
            assert!(depth > 0 || len == 0);
            depth += 1;
            writeln!(buf, "{indent}{kind:?}").unwrap();
            indent.push_str("  ");
        }
        crate::StrStep::Exit => {
            assert!(depth > 0);
            depth -= 1;
            indent.pop();
            indent.pop();
        }
        crate::StrStep::Error { msg, pos } => {
            assert!(depth > 0);
            errors.push(format!("error {pos}: {msg}\n"))
        }
    });
    assert_eq!(
        len,
        text.len(),
        "didn't parse all text.\nParsed:\n{}\n\nAll:\n{}\n",
        &text[..len],
        text
    );

    for (token, msg) in lexed.errors() {
        let pos = lexed.text_start(token);
        errors.push(format!("error {pos}: {msg}\n"));
    }

    let has_errors = !errors.is_empty();
    for e in errors {
        buf.push_str(&e);
    }
    (buf, has_errors)
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TestCase {
    ath: PathBuf,
    entry: EntryPoint,
    text: String,
}

fn entry_point_from_str(s: &str) -> EntryPoint {
    match s {
        "expr" => EntryPoint::Expr,
        "file" => EntryPoint::SourceFile,
        "pat" => EntryPoint::Pat,
        "ded" => EntryPoint::Ded,
        "dir" => EntryPoint::Dir,
        _ => panic!("unknown entry point"),
    }
}

impl TestCase {
    fn read(path: &Path) -> TestCase {
        let entry = entry_point_from_str(
            path.parent()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap(),
        );
        if path.extension().unwrap_or_default() == "ath" {
            let ath = path.into();
            let text = fs::read_to_string(&ath).unwrap();
            TestCase { ath, text, entry }
        } else {
            panic!("unknown file extension");
        }
    }

    fn list(path: &str) -> Vec<TestCase> {
        let crate_root_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let test_data_dir = crate_root_dir.join("test_data");
        let dir = test_data_dir.join(path);

        let mut res = Vec::new();
        let read_dir = fs::read_dir(&dir)
            .unwrap_or_else(|err| panic!("can't `read_dir` {}: {err}", dir.display()));
        for file in read_dir {
            let file = file.unwrap();
            let path = file.path();
            if path.is_dir() {
                let entry = entry_point_from_str(path.file_name().unwrap().to_str().unwrap());
                for case in Self::list(path.to_str().unwrap()) {
                    res.push(TestCase { entry, ..case })
                }
                continue;
            }
            if path.extension().unwrap_or_default() == "ath" {
                let ath = path;
                let text = fs::read_to_string(&ath).unwrap();
                res.push(TestCase {
                    ath,
                    text,
                    entry: EntryPoint::SourceFile,
                });
            }
        }
        res.sort();
        res
    }
}
