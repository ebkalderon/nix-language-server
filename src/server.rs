pub use self::service::{ExitReceiver, LspService};
pub use self::stdio::Server;

use futures::future::FutureResult;
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Result};
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

    #[rpc(name = "shutdown")]
    fn shutdown(&self) -> FutureResult<(), RpcError>;

    // Text synchronization

    #[rpc(name = "textDocument/didOpen", raw_params)]
    fn did_open(&self, params: Params);

    #[rpc(name = "textDocument/didSave", raw_params)]
    fn did_save(&self, params: Params);

    #[rpc(name = "textDocument/didChange", raw_params)]
    fn did_change(&self, params: Params);

    // Language features

    #[rpc(name = "textDocument/hover", raw_params)]
    fn hover(&self, params: Params) -> FutureResult<Option<Hover>, RpcError>;

    #[rpc(name = "textDocument/documentHighlight", raw_params)]
    fn highlight(&self, params: Params) -> FutureResult<Option<Vec<DocumentHighlight>>, RpcError>;
}
