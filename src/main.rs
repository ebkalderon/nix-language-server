use nix_language_server::{self, Args};
use structopt::StructOpt;

fn main() {
    let args = Args::from_args();
    nix_language_server::run(args);
}
