//! Defines `Fixture` -- a convenient way to describe the initial state of
//! the root database from a single string.
//!
//! Fixtures are strings containing athena source code with optional metadata.
//! A fixture without metadata is parsed into a single source file.
//! Use this to test functionality local to one file.
//!
//! Simple Example:
//! ```ignore
//! r#"
//! module Foo {
//!     domain Bar
//! }
//! "#
//! ```
//!
//! Metadata can be added to a fixture after a `//-` comment.
//! The basic form is specifying filenames,
//! which is also how to define multiple files in a single test fixture
//!
//! Example using two files:
//! ```ignore
//! r#"
//! //- /main.ath
//! load "foo.ath"
//! module Bar {
//!     domain Baz
//! }
//!
//! //- /foo.ath
//! define thing := 1
//! "#
//! ```
//!
//! Metadata allows specifying:
//! - environment variables via `env:PATH=/bin,ATHENA_HOME=/here`
//!
//! Example using all available metadata:
//! ```ignore
//! "
//! //- /main.ath env:ATHENA_HOME=path/to,OTHER=foo
//! domain Foo
//! "
//! ```

use rustc_hash::FxHashMap;
use util::trim_indent;

#[derive(Debug, Eq, PartialEq)]
pub struct Fixture {
    pub path: String,
    pub text: String,
    pub env: FxHashMap<String, String>,
}

impl Fixture {
    /// Parses text which looks like this:
    ///
    ///  ```not_rust
    ///  //- some meta
    ///  line 1
    ///  line 2
    ///  //- other meta
    ///  ```
    ///
    /// Fixture can also start with a proc_macros and minicore declaration(in that order):
    ///
    /// ```
    /// //- proc_macros: identity
    /// //- minicore: sized
    /// ```
    ///
    /// That will include predefined proc macros and a subset of `libcore` into the fixture, see
    /// `minicore.rs` for what's available.
    pub fn parse(ath_fixture: &str) -> Vec<Fixture> {
        let fixture = trim_indent(ath_fixture);
        let fixture = fixture.as_str();
        let mut res: Vec<Fixture> = Vec::new();

        let default = if fixture.contains("//-") {
            None
        } else {
            Some("//- /main.rs")
        };

        for (ix, line) in default
            .into_iter()
            .chain(fixture.split_inclusive('\n'))
            .enumerate()
        {
            if line.contains("//-") {
                assert!(
                    line.starts_with("//-"),
                    "Metadata line {ix} has invalid indentation. \
                     All metadata lines need to have the same indentation.\n\
                     The offending line: {line:?}"
                );
            }

            if line.starts_with("//-") {
                let meta = Fixture::parse_meta_line(line);
                res.push(meta);
            } else {
                if line.starts_with("// ")
                    && line.contains(':')
                    && !line.contains("::")
                    && !line.contains('.')
                    && line.chars().all(|it| !it.is_uppercase())
                {
                    panic!("looks like invalid metadata line: {line:?}");
                }

                if let Some(entry) = res.last_mut() {
                    entry.text.push_str(line);
                }
            }
        }

        res
    }

    //- /lib.rs crate:foo deps:bar,baz cfg:foo=a,bar=b env:OUTDIR=path/to,OTHER=foo
    fn parse_meta_line(meta: &str) -> Fixture {
        assert!(meta.starts_with("//-"));
        let meta = meta["//-".len()..].trim();
        let components = meta.split_ascii_whitespace().collect::<Vec<_>>();

        let path = components[0].to_string();
        assert!(
            path.starts_with('/'),
            "fixture path does not start with `/`: {path:?}"
        );

        let mut env = FxHashMap::default();
        for component in components[1..].iter() {
            let (key, value) = component
                .split_once(':')
                .unwrap_or_else(|| panic!("invalid meta line: {meta:?}"));
            match key {
                "env" => {
                    for key in value.split(',') {
                        if let Some((k, v)) = key.split_once('=') {
                            env.insert(k.into(), v.into());
                        }
                    }
                }
                _ => panic!("bad component: {component:?}"),
            }
        }

        Fixture {
            path,
            text: String::new(),
            env,
        }
    }
}

#[test]
#[should_panic]
fn parse_fixture_checks_further_indented_metadata() {
    Fixture::parse(
        r"
        //- /lib.rs
          mod bar;

          fn foo() {}
          //- /bar.rs
          pub fn baz() {}
          ",
    );
}

#[test]
fn parse_fixture_gets_full_meta() {
    let parsed = Fixture::parse(
        r#"
//- /main.ath env:FOO=path/to,OTHER=foo
domain D
"#,
    );
    let meta = &parsed[0];
    assert_eq!("domain D\n", meta.text);

    assert_eq!(2, meta.env.len());
}
