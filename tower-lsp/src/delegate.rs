use futures::future;
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{BoxFuture, Error, Result};
use jsonrpc_derive::rpc;
use log::{error, trace};
use lsp_types::*;

use super::{LanguageServer, Printer};

#[rpc(server)]
pub trait LanguageServerCore {
    // Initialization

    #[rpc(name = "initialize", raw_params)]
    fn initialize(&self, params: Params) -> Result<InitializeResult>;

    #[rpc(name = "initialized", raw_params)]
    fn initialized(&self, params: Params);

    #[rpc(name = "shutdown", returns = "()")]
    fn shutdown(&self) -> BoxFuture<()>;

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
        trace!("received `initialize` request: {:?}", params);
        let params: InitializeParams = params.parse()?;
        self.server.initialize(params)
    }

    fn initialized(&self, params: Params) {
        trace!("received `initialized` notification: {:?}", params);
        match params.parse::<InitializedParams>() {
            Ok(params) => self.server.initialized(&self.printer, params),
            Err(err) => error!("invalid parameters for `initialized`: {:?}", err),
        }
    }

    fn shutdown(&self) -> BoxFuture<()> {
        trace!("received `shutdown` request");
        Box::new(self.server.shutdown())
    }

    fn did_open(&self, params: Params) {
        trace!("received `textDocument/didOpen` notification: {:?}", params);
        match params.parse::<DidOpenTextDocumentParams>() {
            Ok(params) => self.server.did_open(&self.printer, params),
            Err(err) => error!("invalid parameters for `textDocument/didOpen`: {:?}", err),
        }
    }

    fn did_save(&self, params: Params) {
        trace!("received `textDocument/didSave` notification: {:?}", params);
        match params.parse::<DidSaveTextDocumentParams>() {
            Ok(params) => self.server.did_save(&self.printer, params),
            Err(err) => error!("invalid parameters for `textDocument/didSave`: {:?}", err),
        }
    }

    fn did_change(&self, params: Params) {
        trace!(
            "received `textDocument/didChange` notification: {:?}",
            params
        );
        match params.parse::<DidChangeTextDocumentParams>() {
            Ok(params) => self.server.did_change(&self.printer, params),
            Err(err) => error!("invalid parameters for `textDocument/didChange`: {:?}", err),
        }
    }

    fn hover(&self, params: Params) -> BoxFuture<Option<Hover>> {
        trace!("received `textDocument/hover` request: {:?}", params);
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => Box::new(self.server.hover(params)),
            Err(err) => Box::new(future::err(Error::invalid_params_with_details(
                "invalid parameters",
                err,
            ))),
        }
    }

    fn highlight(&self, params: Params) -> BoxFuture<Option<Vec<DocumentHighlight>>> {
        trace!("received `textDocument/highlight` request: {:?}", params);
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => Box::new(self.server.highlight(params)),
            Err(err) => Box::new(future::err(Error::invalid_params_with_details(
                "invalid parameters",
                err,
            ))),
        }
    }
}
