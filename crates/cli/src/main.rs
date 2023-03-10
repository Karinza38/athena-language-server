use std::path::PathBuf;

use clap::Parser;

#[derive(clap::Parser)]
enum Cli {
    RegressionTest(RegressionTestArgs),
}
#[derive(clap::Args)]
struct RegressionTestArgs {
    path: PathBuf,
}

#[derive(clap::Subcommand)]
enum Command {
    RegressionTest,
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let cli = Cli::parse();

    match cli {
        Cli::RegressionTest(args) => {
            let mut g = args.path.to_string_lossy().to_string();
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
    }

    Ok(())
}
