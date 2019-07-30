#![forbid(unsafe_code)]

use env_logger;
use jsonrpc_core::IoHandler;
use log::info;
use structopt::StructOpt;
use tower::ServiceBuilder;

use crate::server::Server;

mod backend;
pub mod protocol;
mod server;

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

#[derive(Debug, StructOpt)]
pub struct Args {
    /// Enable interactive mode
    #[structopt(short = "i", long = "interactive")]
    interactive: bool,
}

pub fn run(args: Args) {
    env_logger::init();
    info!("Nix Language Server {}", env!("CARGO_PKG_VERSION"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let server = Server::new(stdin, stdout).serve();

    tokio::run(server);
}
