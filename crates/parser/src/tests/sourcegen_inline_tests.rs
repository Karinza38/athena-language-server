//! This module greps parser's code for specially formatted comments and turns
//! them into tests.

use std::{
    collections::HashMap,
    fs, iter,
    path::{Path, PathBuf},
};

#[test]
fn sourcegen_parser_tests() {
    let grammar_dir = sourcegen::project_root().join(Path::new("crates/parser/src/grammar"));
    let tests = tests_from_dir(&grammar_dir);
    let mut up_to_date = true;
    install_tests(
        &tests.ok,
        "crates/parser/test_data/parser/inline/ok",
        &mut up_to_date,
    );
    install_tests(
        &tests.err,
        "crates/parser/test_data/parser/inline/err",
        &mut up_to_date,
    );

    fn install_tests(tests: &HashMap<String, Test>, into: &str, up_to_date: &mut bool) {
        let tests_dir = sourcegen::project_root().join(into);
        if !tests_dir.is_dir() {
            fs::create_dir_all(&tests_dir).unwrap();
        }
        // ok is never actually read, but it needs to be specified to create a Test in existing_tests
        let existing = existing_tests(&tests_dir, true);
        for t in existing.keys().filter(|&t| !tests.contains_key(t)) {
            panic!("Test is deleted: {t} ({tests:?})");
        }

        let mut new_idx = existing.len() + 1;
        for (name, test) in tests {
            let path = match existing.get(name) {
                Some((path, _test)) => path.clone(),
                None => {
                    let file_name = format!("{new_idx:04}_{name}.{EXT}");
                    new_idx += 1;
                    tests_dir.join(&test.entry).join(file_name)
                }
            };
            let file_up_to_date = sourcegen::ensure_file_contents(&path, &test.text);
            *up_to_date = file_up_to_date && *up_to_date;
        }
    }

    if !up_to_date {
        sourcegen::fail_sourcegen_test();
    }
}

#[derive(Debug)]
struct Test {
    entry: String,
    name: String,
    text: String,
    ok: bool,
}

#[derive(Default, Debug)]
struct Tests {
    ok: HashMap<String, Test>,
    err: HashMap<String, Test>,
}

struct TestKind {
    name: String,
    entry: String,
    ok: bool,
}
fn test_kind(first_line: &str) -> Option<TestKind> {
    /// returns (entry, name)
    fn parse_rest(rest: &str) -> (String, &str) {
        let mut entry = String::new();
        let bytes = rest.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let b = bytes[i];
            if b == b')' {
                i += 1;
                break;
            }
            entry.push(char::from(b));
            i += 1;
        }
        let name = rest[i..].trim();
        (entry, name)
    }
    if let Some(name) = first_line.strip_prefix("test ") {
        Some(TestKind {
            entry: "file".into(),
            name: name.to_string(),
            ok: true,
        })
    } else if let Some(name) = first_line.strip_prefix("test_err ") {
        Some(TestKind {
            entry: "file".into(),
            name: name.to_string(),
            ok: false,
        })
    } else if let Some(rest) = first_line.strip_prefix("test(") {
        let (entry, name) = parse_rest(rest);
        Some(TestKind {
            entry,
            name: name.to_string(),
            ok: true,
        })
    } else if let Some(rest) = first_line.strip_prefix("test_err(") {
        let (entry, name) = parse_rest(rest);
        Some(TestKind {
            entry,
            name: name.to_string(),
            ok: false,
        })
    } else {
        None
    }
}

fn collect_tests(s: &str) -> Vec<Test> {
    let mut res = Vec::new();
    for comment_block in sourcegen::CommentBlock::extract_untagged(s) {
        let first_line = &comment_block.contents[0];
        let Some(TestKind { name, entry, ok}) = test_kind(first_line) else {
            continue;
        };
        let text: String = comment_block.contents[1..]
            .iter()
            .cloned()
            .chain(iter::once(String::new()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!text.trim().is_empty() && text.ends_with('\n'));
        res.push(Test {
            name,
            entry,
            text,
            ok,
        })
    }
    res
}

fn tests_from_dir(dir: &Path) -> Tests {
    let mut res = Tests::default();
    for entry in sourcegen::list_rust_files(dir) {
        process_file(&mut res, entry.as_path());
    }
    let grammar_rs = dir.parent().unwrap().join("grammar.rs");
    process_file(&mut res, &grammar_rs);
    return res;

    fn process_file(res: &mut Tests, path: &Path) {
        let text = fs::read_to_string(path).unwrap();

        for test in collect_tests(&text) {
            if test.ok {
                if let Some(old_test) = res.ok.insert(test.name.clone(), test) {
                    panic!("Duplicate test: {}", old_test.name);
                }
            } else if let Some(old_test) = res.err.insert(test.name.clone(), test) {
                panic!("Duplicate test: {}", old_test.name);
            }
        }
    }
}

const EXT: &str = "ath";

fn existing_tests(dir: &Path, ok: bool) -> HashMap<String, (PathBuf, Test)> {
    let mut res = HashMap::default();
    for file in fs::read_dir(dir).unwrap() {
        let file = file.unwrap();
        let path = file.path();
        if path.is_dir() {
            res.extend(existing_tests(&path, ok));
            continue;
        }
        let parent = path.parent().unwrap();
        if path.extension().unwrap_or_default() != EXT {
            continue;
        }
        let name = {
            let file_name = path.file_name().unwrap().to_str().unwrap();
            file_name[5..file_name.len() - EXT.len() - 1].to_string()
        };
        let text = fs::read_to_string(&path).unwrap();
        let test = Test {
            name: name.clone(),
            text,
            ok,
            entry: parent.as_os_str().to_string_lossy().into(),
        };
        if let Some(old) = res.insert(name, (path, test)) {
            println!("Duplicate test: {old:?}");
        }
    }
    res
}
