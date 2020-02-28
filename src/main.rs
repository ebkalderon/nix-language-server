use nix_language_server::{self, Args};
use structopt::StructOpt;

#[tokio::main]
async fn main() {
    let args = Args::from_args();
    nix_language_server::run(args).await;
}
