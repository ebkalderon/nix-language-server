#![forbid(unsafe_code)]

use std::process;

use env_logger;
use log::{error, info};
use structopt::StructOpt;

use crate::backend::Nix;
use crate::server::{LspService, Server};

mod backend;
mod server;

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

#[derive(Debug, StructOpt)]
pub struct Args {
    /// Enable interactive mode
    #[structopt(short = "i", long = "interactive")]
    interactive: bool,
}

pub fn run(_args: Args) {
    env_logger::init();
    info!("Nix Language Server {}", env!("CARGO_PKG_VERSION"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let backend = Nix::new().unwrap_or_else(|err| {
        error!("backend error: {}", err);
        process::exit(1);
    });

    let service = LspService::new(backend);
    let handle = service.close_handle();
    let server = Server::new(stdin, stdout).serve(service);

    tokio::run(handle.run_until_exit(server));
}
