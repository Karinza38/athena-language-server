mod codegen;

pub(crate) type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

use pico_args::Arguments;

fn subcommand_help() {
    eprintln!("available subcommands:");
    eprintln!("  codegen");
}

fn main() -> Result<(), Error> {
    let mut args = Arguments::from_env();
    let subcommand = args.subcommand()?.unwrap_or_else(|| {
        eprintln!("expected a subcommand");
        subcommand_help();
        std::process::exit(1);
    });

    match subcommand.as_str() {
        "codegen" => codegen::ast(),
        _ => {
            eprintln!("unknown subcommand: {}", subcommand);
            subcommand_help();
            std::process::exit(1);
        }
    }?;

    Ok(())
}
