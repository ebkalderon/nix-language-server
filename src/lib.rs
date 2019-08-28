#![forbid(unsafe_code)]

use log::info;
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

    let (service, messages) = LspService::new(Nix::new());
    let handle = service.close_handle();
    let server = Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service);

    tokio::run(handle.run_until_exit(server));
}
