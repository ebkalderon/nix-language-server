use futures::future::{self, FutureResult};
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{BoxFuture, Error, Result};
use jsonrpc_derive::rpc;
use log::error;
use lsp_types::*;

use super::{LanguageServer, Printer};

#[rpc]
pub trait LanguageServerCore {
    // Initialization

    #[rpc(name = "initialize", raw_params)]
    fn initialize(&self, params: Params) -> Result<InitializeResult>;

    #[rpc(name = "initialized", raw_params)]
    fn initialized(&self, params: Params);

    #[rpc(name = "shutdown")]
    fn shutdown(&self) -> FutureResult<(), Error>;

    // Text synchronization

    #[rpc(name = "textDocument/didOpen", raw_params)]
    fn did_open(&self, params: Params);

    #[rpc(name = "textDocument/didSave", raw_params)]
    fn did_save(&self, params: Params);

    #[rpc(name = "textDocument/didChange", raw_params)]
    fn did_change(&self, params: Params);

    // Language features

    #[rpc(name = "textDocument/hover", raw_params, returns = "Option<Hover>")]
    fn hover(&self, params: Params) -> BoxFuture<Option<Hover>>;

    #[rpc(
        name = "textDocument/documentHighlight",
        raw_params,
        returns = "Option<Vec<DocumentHighlight>>"
    )]
    fn highlight(&self, params: Params) -> BoxFuture<Option<Vec<DocumentHighlight>>>;
}

#[derive(Debug)]
pub struct Delegate<T> {
    server: T,
    printer: Printer,
}

impl<T: LanguageServer> Delegate<T> {
    pub fn new(server: T, printer: Printer) -> Self {
        Delegate { server, printer }
    }
}

impl<T: LanguageServer> LanguageServerCore for Delegate<T> {
    fn initialize(&self, params: Params) -> Result<InitializeResult> {
        let params: InitializeParams = params.parse()?;
        self.server.initialize(params)
    }

    fn initialized(&self, params: Params) {
        match params.parse::<InitializedParams>() {
            Ok(params) => self.server.initialized(params),
            Err(err) => error!("invalid parameters for `initialized`: {:?}", err),
        }
    }

    fn shutdown(&self) -> FutureResult<(), Error> {
        self.server.shutdown()
    }

    fn did_open(&self, params: Params) {
        match params.parse::<DidOpenTextDocumentParams>() {
            Ok(params) => self.server.did_open(&self.printer, params),
            Err(err) => error!("invalid parameters for `textDocument/didOpen`: {:?}", err),
        }
    }

    fn did_save(&self, params: Params) {
        match params.parse::<DidSaveTextDocumentParams>() {
            Ok(params) => self.server.did_save(params),
            Err(err) => error!("invalid parameters for `textDocument/didSave`: {:?}", err),
        }
    }

    fn did_change(&self, params: Params) {
        match params.parse::<DidChangeTextDocumentParams>() {
            Ok(params) => self.server.did_change(&self.printer, params),
            Err(err) => error!("invalid parameters for `textDocument/didChange`: {:?}", err),
        }
    }

    fn hover(&self, params: Params) -> BoxFuture<Option<Hover>> {
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => self.server.hover(params),
            Err(err) => Box::new(future::err(Error::invalid_params_with_details(
                "invalid parameters",
                err,
            ))),
        }
    }

    fn highlight(&self, params: Params) -> BoxFuture<Option<Vec<DocumentHighlight>>> {
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => self.server.highlight(params),
            Err(err) => Box::new(future::err(Error::invalid_params_with_details(
                "invalid parameters",
                err,
            ))),
        }
    }
}
