use std::{
    env,
    path::{Path, PathBuf},
};

use clap::Parser;
use syntax::SyntaxKind;
use xshell::cmd;

#[derive(clap::Parser)]
enum Cli {
    RegressionTest { path: PathBuf },
    FindDeadSyntaxKinds,
}
#[derive(clap::Args)]
struct RegressionTestArgs {}

#[derive(serde::Deserialize)]
struct JsonPath {
    text: String,
}

#[derive(serde::Deserialize)]
#[serde(tag = "type", content = "data")]
#[serde(rename_all = "camelCase")]
enum RipgrepResult {
    Match { path: JsonPath },
    Begin {},
    End {},
    Summary {},
}

fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(2)
    .unwrap()
    .to_path_buf()
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let cli = Cli::parse();

    match cli {
        Cli::RegressionTest { path } => {
            let mut g = path.to_string_lossy().to_string();
            let mut pass = 0;
            let mut fail = 0;
            let mut panic = 0;
            g.push_str("/**/*.ath");
            for pth in glob::glob(&g)? {
                let path = pth?;

                let contents = std::fs::read_to_string(&path)?;

                let parsed = std::panic::catch_unwind(|| syntax::SourceFile::parse(&contents));

                let parsed = match parsed {
                    Ok(parsed) => parsed,
                    Err(e) => {
                        panic += 1;
                        println!("test PANIC: {:?} -- {e:?}", path);
                        continue;
                    }
                };

                if parsed.errors().is_empty() {
                    pass += 1;
                    println!("ok: {:?}", path);
                } else {
                    fail += 1;
                    println!("test fail: {:?}", path);
                    println!("{}", parsed.debug_dump());
                }

                println!("regression test: {:?}", path);
            }
            eprintln!("{pass} pass, {fail} fail, {panic} panic / {}", pass + fail)
        }
        Cli::FindDeadSyntaxKinds => {
            let sh = xshell::Shell::new()?;
            sh.change_dir(project_root());
            let mut dead = Vec::new();
            for i in 0..(SyntaxKind::__LAST as u16) {
                let kind = SyntaxKind::from(i);
                let kind_dbg = if !kind.is_node() && !kind.is_trivia() {
                    continue;
                } else {
                    format!("{kind:?}")
                };
                let output = cmd!(sh, "rg --json {kind_dbg} crates")
                    .ignore_status()
                    .read()?;
                let mut other_match = false;
                for line in output.lines() {
                    let line: RipgrepResult = serde_json::from_str(line)?;
                    match line {
                        RipgrepResult::Match { path } => {
                            if !path.text.contains("generated")
                                && path.text.ends_with(".rs")
                                && !kind.is_keyword()
                            {
                                other_match = true;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                if !other_match {
                    dead.push(kind);
                }
            }
            println!("{:?}", dead);
        }
    }

    Ok(())
}
