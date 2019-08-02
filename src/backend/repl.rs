use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::io::{BufRead, BufReader, ErrorKind, Read, Write};
use std::process::{Child, ChildStderr, ChildStdin, ChildStdout, Command, Stdio};

use log::{error, info, warn};
use rexpect::session::{self, PtySession, ReadUntil};

use crate::Error;

const NIX_INSTANTIATE_COMMAND: &str = "nix-instantiate";
const NIX_REPL_COMMAND: &str = "nix repl";
const NIX_REPL_TIMEOUT_MS: Option<u64> = Some(3000);
const PROMPT_STRING: &str = "nix-repl> ";

pub struct Repl {
    repl: PtySession,
    instantiate: Command,
}

impl Repl {
    pub fn new() -> Result<Self, Error> {
        let mut repl =
            session::spawn(NIX_REPL_COMMAND, NIX_REPL_TIMEOUT_MS).map_err(|e| e.to_string())?;
        repl.exp_string(PROMPT_STRING).map_err(|e| e.to_string())?;
        repl.send_line(":l <nixpkgs>").map_err(|e| e.to_string())?;

        let mut instantiate = Command::new(NIX_INSTANTIATE_COMMAND);
        instantiate
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .args(&["--eval", "--strict", "-"]);

        Ok(Repl { repl, instantiate })
    }

    pub fn diagnostics(&mut self, file_input: &str) -> Result<Vec<String>, Error> {
        let mut child = self.instantiate.spawn()?;

        let mut stdin = child.stdin.as_mut().ok_or("failed to get stdin")?;
        write!(&mut stdin, "{}", file_input)?;
        let output = child.wait_with_output()?;

        if output.status.success() {
            Ok(Vec::new())
        } else {
            let stderr = String::from_utf8(output.stderr)?;
            Ok(stderr.lines().map(Into::into).collect())
        }
    }

    pub fn completions(&mut self, expr: &str) -> Result<Vec<String>, Error> {
        self.repl
            .exp_string(PROMPT_STRING)
            .map_err(|e| e.to_string())?;

        self.repl
            .send(&format!("{}\t\t", expr))
            .map_err(|e| e.to_string())?;
        self.repl.flush().map_err(|e| e.to_string())?;

        let (output, _) = self
            .repl
            .exp_any(vec![
                ReadUntil::String(PROMPT_STRING.into()),
                ReadUntil::String(format!("{}\u{7}\u{7}", expr)),
            ])
            .map_err(|e| e.to_string())?;

        let results = if output.is_empty() {
            Vec::new()
        } else {
            output.split_whitespace().skip(1).map(Into::into).collect()
        };

        self.repl.send_control('c').map_err(|e| e.to_string())?;

        Ok(results)
    }
}

impl Debug for Repl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        fmt.debug_struct(stringify!(Repl))
            .field("repl", &stringify!(PtySession))
            .field("instantiate", &self.instantiate)
            .finish()
    }
}

impl Drop for Repl {
    fn drop(&mut self) {
        if let Err(err) = self.repl.send_line(":q").and_then(|_| self.repl.flush()) {
            error!("failed to kill `nix repl` process: {}", err);
        }
    }
}
