use std::fmt;

use xshell::{cmd, Shell};

use crate::{CodeBin, Result};

use super::Install;

enum InstallTarget {
    Server,
    Client,
    Both,
}

impl Install {
    fn target(&self) -> InstallTarget {
        if self.server && self.client {
            InstallTarget::Both
        } else if self.server {
            InstallTarget::Server
        } else if self.client {
            InstallTarget::Client
        } else {
            InstallTarget::Both
        }
    }
}

impl fmt::Display for CodeBin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Code => write!(f, "code"),
            Self::Codium => write!(f, "codium"),
            Self::CodeInsiders => write!(f, "code-insiders"),
        }
    }
}

fn install_client(sh: &Shell, code_bin: &str) -> Result<()> {
    let _cd = sh.push_dir("./client");
    cmd!(sh, "npm run package --scripts-prepend-node-path").run()?;

    cmd!(
        sh,
        "{code_bin} --install-extension athena-language-server.vsix"
    )
    .run()?;

    let exts = cmd!(sh, "{code_bin} --list-extensions").read()?;

    if !exts.contains("athena-language-support-vscode") {
        return crate::error!("failed to install the extension");
    }

    Ok(())
}

fn install_server(sh: &Shell) -> Result<()> {
    cmd!(
        sh,
        "cargo install --path crates/athena-language-server --force"
    )
    .run()?;

    Ok(())
}

pub(crate) fn run(sh: &Shell, options: Install) -> Result<()> {
    let target = options.target();
    let code_bin = options.bin.to_string();
    match target {
        InstallTarget::Server => install_server(sh),
        InstallTarget::Client => install_client(sh, &code_bin),
        InstallTarget::Both => {
            install_server(sh)?;
            install_client(sh, &code_bin)
        }
    }
}
