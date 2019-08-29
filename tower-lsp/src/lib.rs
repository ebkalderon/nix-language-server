//! Language Server Protocol (LSP) server abstraction for Tower.
//!
//! # Example
//!
//! ```rust
//! # use futures::future::{self, Future};
//! # use jsonrpc_core::{BoxFuture, Result};
//! # use tower_lsp::lsp_types::*;
//! # use tower_lsp::{LanguageServer, LspService, Printer, Server};
//! #
//! #[derive(Debug, Default)]
//! struct Backend;
//!
//! impl LanguageServer for Backend {
//!     type ShutdownFuture = BoxFuture<()>;
//!     type HighlightFuture = BoxFuture<Option<Vec<DocumentHighlight>>>;
//!     type HoverFuture = BoxFuture<Option<Hover>>;
//!
//!     fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
//!         Ok(InitializeResult::default())
//!     }
//!
//!     fn initialized(&self, printer: &Printer, _: InitializedParams) {
//!         printer.log_message(MessageType::Info, "server initialized!");
//!     }
//!
//!     fn shutdown(&self) -> Self::ShutdownFuture {
//!         Box::new(future::ok(()))
//!     }
//!
//!     fn did_open(&self, printer: &Printer, _: DidOpenTextDocumentParams) {
//!         printer.log_message(MessageType::Info, "file opened!");
//!     }
//!
//!     fn did_save(&self, printer: &Printer, _: DidSaveTextDocumentParams) {
//!         printer.log_message(MessageType::Info, "file saved!");
//!     }
//!
//!     fn did_change(&self, printer: &Printer, _: DidChangeTextDocumentParams) {
//!         printer.log_message(MessageType::Info, "file changed!");
//!     }
//!
//!     fn highlight(&self, _: TextDocumentPositionParams) -> Self::HighlightFuture {
//!         Box::new(future::ok(None))
//!     }
//!
//!     fn hover(&self, _: TextDocumentPositionParams) -> Self::HoverFuture {
//!         Box::new(future::ok(None))
//!     }
//! }
//!
//! fn main() {
//!     let stdin = tokio::io::stdin();
//!     let stdout = tokio::io::stdout();
//!
//!     let (service, messages) = LspService::new(Backend::default());
//!     let handle = service.close_handle();
//!     let server = Server::new(stdin, stdout)
//!         .interleave(messages)
//!         .serve(service);
//!
//!     tokio::run(handle.run_until_exit(server));
//! }
//! ```

pub extern crate lsp_types;

pub use self::delegate::{MessageStream, Printer};
pub use self::service::{ExitReceiver, LspService};
pub use self::stdio::Server;

use futures::Future;
use jsonrpc_core::{Error, Result};
use lsp_types::*;

mod codec;
mod delegate;
mod service;
mod stdio;

pub trait LanguageServer: Send + Sync + 'static {
    type ShutdownFuture: Future<Item = (), Error = Error> + Send;
    type HighlightFuture: Future<Item = Option<Vec<DocumentHighlight>>, Error = Error> + Send;
    type HoverFuture: Future<Item = Option<Hover>, Error = Error> + Send;

    fn initialize(&self, params: InitializeParams) -> Result<InitializeResult>;

    fn initialized(&self, printer: &Printer, params: InitializedParams);

    fn shutdown(&self) -> Self::ShutdownFuture;

    fn did_open(&self, printer: &Printer, params: DidOpenTextDocumentParams);

    fn did_save(&self, printer: &Printer, params: DidSaveTextDocumentParams);

    fn did_change(&self, printer: &Printer, params: DidChangeTextDocumentParams);

    fn highlight(&self, params: TextDocumentPositionParams) -> Self::HighlightFuture;

    fn hover(&self, params: TextDocumentPositionParams) -> Self::HoverFuture;
}

impl<S: ?Sized + LanguageServer> LanguageServer for Box<S> {
    type ShutdownFuture = S::ShutdownFuture;
    type HighlightFuture = S::HighlightFuture;
    type HoverFuture = S::HoverFuture;

    fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        (**self).initialize(params)
    }

    fn initialized(&self, printer: &Printer, params: InitializedParams) {
        (**self).initialized(printer, params);
    }

    fn shutdown(&self) -> Self::ShutdownFuture {
        (**self).shutdown()
    }

    fn did_open(&self, printer: &Printer, params: DidOpenTextDocumentParams) {
        (**self).did_open(printer, params);
    }

    fn did_save(&self, printer: &Printer, params: DidSaveTextDocumentParams) {
        (**self).did_save(printer, params);
    }

    fn did_change(&self, printer: &Printer, params: DidChangeTextDocumentParams) {
        (**self).did_change(printer, params);
    }

    fn highlight(&self, params: TextDocumentPositionParams) -> Self::HighlightFuture {
        (**self).highlight(params)
    }

    fn hover(&self, params: TextDocumentPositionParams) -> Self::HoverFuture {
        (**self).hover(params)
    }
}
