use futures::future::{self, FutureResult, IntoFuture};
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, ErrorCode, Metadata, Result};
use log::{debug, info, trace};
use lsp_types::*;
use tokio;

use crate::server::LanguageServer;

mod ast;

#[derive(Debug)]
pub struct Nix;

impl LanguageServer for Nix {
    fn initialize(&self, params: Params) -> Result<InitializeResult> {
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
    }

    fn did_open(&self, _: Params) {
        tokio::spawn(future::lazy(|| {
            print!("Content-Length: 72\r\n\r\n{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{{}}}}");
            Ok(())
        }));
    }

    fn did_save(&self, _: Params) {}

    fn did_change(&self, _: Params) {}

    fn shutdown(&self) -> FutureResult<(), RpcError> {
        future::ok(())
    }
}
