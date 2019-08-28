pub use self::service::{ExitReceiver, LspService, MessageStream, Printer};
pub use self::stdio::Server;

use futures::future::FutureResult;
use jsonrpc_core::{BoxFuture, Error, Result};
use lsp_types::*;

mod codec;
mod delegate;
mod service;
mod stdio;

pub trait LanguageServer: Send + Sync + 'static {
    fn initialize(&self, params: InitializeParams) -> Result<InitializeResult>;

    fn initialized(&self, params: InitializedParams);

    fn shutdown(&self) -> FutureResult<(), Error>;

    fn did_open(&self, printer: Printer, params: DidOpenTextDocumentParams);

    fn did_save(&self, params: DidSaveTextDocumentParams);

    fn did_change(&self, printer: Printer, params: DidChangeTextDocumentParams);

    fn hover(&self, params: TextDocumentPositionParams) -> BoxFuture<Option<Hover>>;

    fn highlight(
        &self,
        params: TextDocumentPositionParams,
    ) -> BoxFuture<Option<Vec<DocumentHighlight>>>;
}
