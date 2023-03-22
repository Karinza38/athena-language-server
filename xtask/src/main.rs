mod codegen;
mod install;

pub(crate) type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
pub(crate) type Result<T, E = Error> = std::result::Result<T, E>;

use std::{
    env,
    path::{Path, PathBuf},
    str::FromStr,
};

use pico_args::Arguments;

const HELP: &str = "\
cargo-xtask

USAGE:
    cargo xtask <SUBCOMMAND>

SUBCOMMANDS:
    codegen     Generate code for AST
    install     Install the xtask binary. If neither --server nor --client are
                specified, both are installed.
                FLAGS:
                    --server    Install the xtask server
                    --client    Install the xtask client
                    --code-bin  Name of the code binary. One of 'code', 'codium', or 'code-insiders' [default: code] 

FLAGS:
    -h, --help    Prints help information
";

macro_rules! error {
    ($($tt:tt)*) => {
        Err(format!($($tt)*).into())
    }
}

pub(crate) use error;
use xshell::Shell;

#[derive(Debug)]
enum Subcommand {
    Codegen,
    Install(Install),
}

#[derive(Debug)]
struct Install {
    server: bool,
    client: bool,
    bin: CodeBin,
}

#[derive(Clone, Copy, Debug, Default)]
enum CodeBin {
    #[default]
    Code,
    Codium,
    CodeInsiders,
}

impl FromStr for CodeBin {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().trim() {
            "code" => Ok(Self::Code),
            "codium" => Ok(Self::Codium),
            "code-insiders" => Ok(Self::CodeInsiders),
            _ => Err(format!("unknown code binary name: {s}").into()),
        }
    }
}

const RUN_WITH_HELP: &str = "Run with --help for more information.";

impl Subcommand {
    fn parse(mut args: Arguments) -> Result<Self> {
        let Some(subcommand) = args.subcommand()? else {
            return error!("expected a subcommand. {RUN_WITH_HELP}");
        };

        Ok(match subcommand.as_str() {
            "codegen" => Self::Codegen,
            "install" => Self::Install(Install {
                server: args.contains("--server"),
                client: args.contains("--client"),
                bin: args.opt_value_from_str("--code-bin")?.unwrap_or_default(),
            }),
            _ => {
                return error!("unknown subcommand: {subcommand}. {RUN_WITH_HELP}");
            }
        })
    }

    fn run(self) -> Result<()> {
        let shell = Shell::new()?;
        shell.change_dir(project_root());
        match self {
            Self::Codegen => codegen::ast(),
            Self::Install(install) => install::run(&shell, install),
        }
    }
}

fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

fn main() -> Result<(), Error> {
    let mut args = Arguments::from_env();

    if args.contains(["-h", "--help"]) {
        println!("{}", HELP);
        return Ok(());
    }

    let subcommand = Subcommand::parse(args)?;

    subcommand.run()?;

    Ok(())
}
