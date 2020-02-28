#![forbid(unsafe_code)]

use log::info;
use structopt::StructOpt;
use tower_lsp::{LspService, Server};

use crate::backend::Nix;

mod backend;

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

#[derive(Debug, StructOpt)]
pub struct Args {
    /// Enable interactive mode
    #[structopt(short = "i", long = "interactive")]
    interactive: bool,
}

pub async fn run(_args: Args) {
    env_logger::init();
    info!("Nix Language Server {}", env!("CARGO_PKG_VERSION"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(Nix::new());
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
