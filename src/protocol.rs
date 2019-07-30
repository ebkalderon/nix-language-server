use futures::future::FutureResult;
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Result};
use jsonrpc_derive::rpc;
use lsp_types::*;

#[rpc]
pub trait LanguageServer: Sized {
    #[rpc(name = "initialize", raw_params)]
    fn initialize(&self, params: Params) -> Result<InitializeResult>;

    #[rpc(name = "initialized", raw_params)]
    fn initialized(&self, params: Params);

    #[rpc(name = "shutdown")]
    fn shutdown(&self) -> FutureResult<(), RpcError>;
}
