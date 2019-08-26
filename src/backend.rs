use std::collections::HashMap;
use std::sync::Mutex;

use codespan::{FileId, Files};
use codespan_lsp::{make_lsp_diagnostic, range_to_byte_span};
use futures::future::{self, FutureResult};
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Result as RpcResult};
use log::{debug, error, info};
use lsp_types::*;
use nix_parser::ast::SourceFile;

use crate::server::LanguageServer;

#[derive(Debug)]
struct State {
    sources: HashMap<Url, FileId>,
    files: Files,
}

#[derive(Debug)]
pub struct Nix {
    state: Mutex<State>,
}

impl Nix {
    pub fn new() -> Self {
        Nix {
            state: Mutex::new(State {
                sources: HashMap::new(),
                files: Files::new(),
            }),
        }
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
    }

    fn shutdown(&self) -> FutureResult<(), RpcError> {
        future::ok(())
    }

    fn did_open(&self, params: Params) {
        match params.parse::<DidOpenTextDocumentParams>() {
            Err(err) => error!("{}", err),
            Ok(params) => {
                let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
                let (source, id) = get_or_insert_source(&mut state, &params.text_document);
                let diags = get_diagnostics(&state, id, &source);
                let s = serde_json::to_string(&diags).unwrap();
                let msg = format!("{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{}}}", s);
                print!("Content-Length: {}\r\n\r\n{}", msg.len(), msg);
            }
        }
    }

    fn did_save(&self, _: Params) {}

    fn did_change(&self, params: Params) {
        match params.parse::<DidChangeTextDocumentParams>() {
            Err(err) => error!("{}", err),
            Ok(params) => {
                let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
                let (source, id) =
                    reload_source(&mut state, &params.text_document, params.content_changes);
                info!("updated source is: \"{}\"", source);
                let diags = get_diagnostics(&state, id, &source);
                let s = serde_json::to_string(&diags).unwrap();
                let msg = format!("{{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{}}}", s);
                print!("Content-Length: {}\r\n\r\n{}", msg.len(), msg);
            }
        }
    }

    fn hover(&self, params: Params) -> FutureResult<Option<Hover>, RpcError> {
        match params.parse::<TextDocumentPositionParams>() {
            Err(err) => future::err(RpcError::invalid_params_with_details("invalid params", err)),
            Ok(params) => {
                debug!("{:?}", params);
                future::ok(None)
            }
        }
    }

    fn highlight(&self, params: Params) -> FutureResult<Option<Vec<DocumentHighlight>>, RpcError> {
        match params.parse::<TextDocumentPositionParams>() {
            Err(err) => future::err(RpcError::invalid_params_with_details("invalid params", err)),
            Ok(params) => {
                debug!("{:?}", params);
                future::ok(None)
            }
        }
    }
}

fn get_or_insert_source(state: &mut State, document: &TextDocumentItem) -> (String, FileId) {
    if let Some(id) = state.sources.get(&document.uri) {
        (state.files.source(*id).to_owned(), *id)
    } else {
        let id = state
            .files
            .add(document.uri.to_string(), document.text.clone());
        state.sources.insert(document.uri.clone(), id);
        (state.files.source(id).to_owned(), id)
    }
}

fn reload_source<'a>(
    state: &mut State,
    document: &VersionedTextDocumentIdentifier,
    changes: Vec<TextDocumentContentChangeEvent>,
) -> (String, FileId) {
    if let Some(id) = state.sources.get(&document.uri) {
        let mut source = state.files.source(*id).to_owned();
        for change in changes {
            if let (None, None) = (change.range, change.range_length) {
                source = change.text;
            } else if let Some(range) = change.range {
                let span = range_to_byte_span(&state.files, *id, &range).unwrap();
                let range = (span.start().to_usize())..(span.end().to_usize());
                source.replace_range(range, &change.text);
            }
        }
        state.files.update(*id, source);
        (state.files.source(*id).to_owned(), *id)
    } else {
        panic!("attempted to reload source that does not exist");
    }
}

fn get_diagnostics(state: &State, id: FileId, source: &str) -> PublishDiagnosticsParams {
    let uri = state
        .sources
        .iter()
        .filter(|(_, v)| **v == id)
        .next()
        .map(|(k, _)| k.clone())
        .unwrap();

    match source.parse::<SourceFile>() {
        Ok(_) => PublishDiagnosticsParams::new(uri, Vec::new()),
        Err(err) => {
            let diagnostics = err.to_diagnostics(id);

            let mut new_diags = Vec::new();
            for diag in diagnostics {
                let diag =
                    make_lsp_diagnostic(&state.files, None, diag, |_| Ok(uri.clone())).unwrap();
                new_diags.push(diag);
            }

            PublishDiagnosticsParams::new(uri, new_diags)
        }
    }
}
