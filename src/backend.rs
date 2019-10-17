//! HACK: All of this.

use std::collections::HashMap;
use std::sync::Mutex;

use codespan::{FileId, Files};
use codespan_lsp::{make_lsp_diagnostic, range_to_byte_span};
use futures::future::{self, FutureResult};
use jsonrpc_core::{BoxFuture, Error, Result as RpcResult};
use log::info;
use nix_parser::ast::SourceFile;
use serde_json::Value;
use tower_lsp::lsp_types::*;
use tower_lsp::{LanguageServer, Printer};

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
    type ShutdownFuture = FutureResult<(), Error>;
    type SymbolFuture = FutureResult<Option<Vec<SymbolInformation>>, Error>;
    type ExecuteFuture = FutureResult<Option<Value>, Error>;
    type CompletionFuture = FutureResult<Option<CompletionResponse>, Error>;
    type HoverFuture = BoxFuture<Option<Hover>>;
    type HighlightFuture = BoxFuture<Option<Vec<DocumentHighlight>>>;

    fn initialize(&self, _: &Printer, _: InitializeParams) -> RpcResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Incremental,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![".".to_string()]),
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

    fn shutdown(&self) -> Self::ShutdownFuture {
        future::ok(())
    }

    fn symbol(&self, _: WorkspaceSymbolParams) -> Self::SymbolFuture {
        future::ok(None)
    }

    fn execute_command(&self, _: &Printer, _: ExecuteCommandParams) -> Self::ExecuteFuture {
        future::ok(None)
    }

    fn completion(&self, _: CompletionParams) -> Self::CompletionFuture {
        future::ok(None)
    }

    fn did_open(&self, printer: &Printer, params: DidOpenTextDocumentParams) {
        let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
        let id = get_or_insert_source(&mut state, &params.text_document);
        let diags = get_diagnostics(&state, &params.text_document.uri, id);
        printer.publish_diagnostics(params.text_document.uri, diags);
    }

    fn did_change(&self, printer: &Printer, params: DidChangeTextDocumentParams) {
        let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
        let id = reload_source(&mut state, &params.text_document, params.content_changes);
        let diags = get_diagnostics(&state, &params.text_document.uri, id);
        printer.publish_diagnostics(params.text_document.uri, diags);
    }

    fn hover(&self, _: TextDocumentPositionParams) -> Self::HoverFuture {
        Box::new(future::ok(None))
    }

    fn document_highlight(&self, _: TextDocumentPositionParams) -> Self::HighlightFuture {
        Box::new(future::ok(None))
    }
}

fn get_or_insert_source(state: &mut State, document: &TextDocumentItem) -> FileId {
    if let Some(id) = state.sources.get(&document.uri) {
        *id
    } else {
        let id = state
            .files
            .add(document.uri.to_string(), document.text.clone());
        state.sources.insert(document.uri.clone(), id);
        id
    }
}

fn reload_source(
    state: &mut State,
    document: &VersionedTextDocumentIdentifier,
    changes: Vec<TextDocumentContentChangeEvent>,
) -> FileId {
    if let Some(id) = state.sources.get(&document.uri) {
        let mut source = state.files.source(*id).to_owned();
        for change in changes {
            if let (None, None) = (change.range, change.range_length) {
                source = change.text;
            } else if let Some(range) = change.range {
                let span = range_to_byte_span(&state.files, *id, &range).unwrap_or_default();
                let range = (span.start().to_usize())..(span.end().to_usize());
                source.replace_range(range, &change.text);
            }
        }
        state.files.update(*id, source);
        *id
    } else {
        panic!("attempted to reload source that does not exist");
    }
}

fn get_diagnostics(state: &State, uri: &Url, id: FileId) -> Vec<Diagnostic> {
    let source = state.files.source(id);
    match source.parse::<SourceFile>() {
        Ok(expr) => {
            info!("parsed expression: {}", expr);
            Vec::new()
        }
        Err(err) => {
            info!("expression has errors: {}", err);
            err.to_diagnostics(id)
                .into_iter()
                .map(|d| make_lsp_diagnostic(&state.files, None, d, |_| Ok(uri.clone())))
                .collect::<Result<Vec<_>, _>>()
                .unwrap()
        }
    }
}
