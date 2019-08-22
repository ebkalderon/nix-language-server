use std::sync::Mutex;

use codespan::CodeMap;
use futures::future::{self, FutureResult};
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Result as RpcResult};
use log::{debug, info};
use lsp_types::*;

use self::repl::Repl;
use crate::server::LanguageServer;
use crate::Error;

mod repl;

#[derive(Debug)]
struct State {
    pub repl: Repl,
    pub codemap: CodeMap,
}

#[derive(Debug)]
pub struct Nix {
    state: Mutex<State>,
}

impl Nix {
    pub fn new() -> Result<Self, Error> {
        Ok(Nix {
            state: Mutex::new(State {
                repl: Repl::new()?,
                codemap: CodeMap::new(),
            }),
        })
    }
}

impl LanguageServer for Nix {
    fn initialize(&self, params: Params) -> RpcResult<InitializeResult> {
        let params: InitializeParams = params.parse()?;
        debug!("initialize with: {:?}", params);
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Incremental,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".into()]),
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: None,
                }),
                hover_provider: Some(true),
                document_formatting_provider: Some(true),
                document_highlight_provider: Some(true),
                document_symbol_provider: Some(true),
                workspace_symbol_provider: Some(true),
                definition_provider: Some(true),
                ..ServerCapabilities::default()
            },
        })
    }

    fn initialized(&self, _: Params) {
        info!("initialized notification received");
        let mut state = self.state.lock().unwrap_or_else(|r| r.into_inner());
        info!("Completions: {:?}", state.repl.completions("__").unwrap());
        info!(
            "Diagnostics: {:?}",
            state.repl.diagnostics("{ foo = bar; baz = 12 } ").unwrap()
        );
    }

    fn shutdown(&self) -> FutureResult<(), RpcError> {
        future::ok(())
    }

    fn did_open(&self, _: Params) {
        print!("Content-Length: 72\r\n\r\n{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{{}}}}");
    }

    fn did_save(&self, _: Params) {}

    fn did_change(&self, _: Params) {}

    fn hover(&self, params: Params) -> FutureResult<Option<Hover>, RpcError> {
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => {
                debug!("{:?}", params);
                future::ok(None)
            }
            Err(err) => future::err(RpcError::invalid_params_with_details("invalid params", err)),
        }
    }

    fn highlight(&self, params: Params) -> FutureResult<Option<Vec<DocumentHighlight>>, RpcError> {
        match params.parse::<TextDocumentPositionParams>() {
            Ok(params) => {
                debug!("{:?}", params);
                future::ok(None)
            }
            Err(err) => future::err(RpcError::invalid_params_with_details("invalid params", err)),
        }
    }
}
