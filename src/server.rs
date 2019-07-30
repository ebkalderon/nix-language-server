pub use self::service::{ExitReceiver, LspService};
pub use self::stdio::Server;

use futures::future::FutureResult;
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Metadata, Result};
use jsonrpc_derive::rpc;
use lsp_types::*;

mod codec;
mod service;
mod stdio;

#[rpc]
pub trait LanguageServer {
    // Initialization

    #[rpc(name = "initialize", raw_params)]
    fn initialize(&self, params: Params) -> Result<InitializeResult>;

    #[rpc(name = "initialized", raw_params)]
    fn initialized(&self, params: Params);

    // Diagnostics

    #[rpc(name = "textDocument/didOpen", raw_params)]
    fn did_open(&self, params: Params);

    #[rpc(name = "textDocument/didSave", raw_params)]
    fn did_save(&self, params: Params);

    #[rpc(name = "textDocument/didChange", raw_params)]
    fn did_change(&self, params: Params);

    /// Initiate a graceful shutdown.
    #[rpc(name = "shutdown")]
    fn shutdown(&self) -> FutureResult<(), RpcError>;
}
