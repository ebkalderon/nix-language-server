#![forbid(unsafe_code)]

use env_logger;
use jsonrpc_core::IoHandler;
use log::debug;
use structopt::StructOpt;

use crate::rpc::{LanguageServerProtocol, Server};
use crate::server::ServerBuilder;

mod parser;
mod rpc;
mod server;

pub type Error = Box<dyn std::error::Error + Send + Sync>;

#[derive(Debug, StructOpt)]
pub struct Args {
    /// Start the language server in interactive mode.
    #[structopt(long = "cli")]
    pub enable_cli: bool,
}

pub fn run(_args: Args) {
    env_logger::init();
    debug!("Nix Language Server {}", env!("CARGO_PKG_VERSION"));
    let mut io = IoHandler::new();
    io.extend_with(Server.to_delegate());
    ServerBuilder::new(io).build();
}
