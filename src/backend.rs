//! HACK: All of this.

use std::collections::HashMap;
use std::sync::Mutex;

use codespan::{FileId, Files};
use codespan_lsp::{make_lsp_diagnostic, range_to_byte_span};
use futures::future::{self, FutureResult};
use jsonrpc_core::{BoxFuture, Error, Result};
use log::info;
use nix_parser::ast::SourceFile;
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
    type HoverFuture = BoxFuture<Option<Hover>>;
    type HighlightFuture = BoxFuture<Option<Vec<DocumentHighlight>>>;

    fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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

    fn initialized(&self, _: &Printer, _: InitializedParams) {
        info!("initialized notification received");
    }

    fn shutdown(&self) -> Self::ShutdownFuture {
        future::ok(())
    }

    fn did_open(&self, printer: &Printer, params: DidOpenTextDocumentParams) {
        let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
        let (source, id) = get_or_insert_source(&mut state, &params.text_document);
        let (_, diags) = get_diagnostics(&state, id, &source);
        printer.publish_diagnostics(params.text_document.uri, diags);
    }

    fn did_save(&self, _: &Printer, _: DidSaveTextDocumentParams) {}

    fn did_change(&self, printer: &Printer, params: DidChangeTextDocumentParams) {
        let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
        let (source, id) = reload_source(&mut state, &params.text_document, params.content_changes);
        let (_, diags) = get_diagnostics(&state, id, &source);
        printer.publish_diagnostics(params.text_document.uri, diags);
    }

    fn hover(&self, _: TextDocumentPositionParams) -> Self::HoverFuture {
        Box::new(future::ok(None))
    }

    fn highlight(&self, _: TextDocumentPositionParams) -> Self::HighlightFuture {
        Box::new(future::ok(None))
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

fn get_diagnostics(state: &State, id: FileId, source: &str) -> (Url, Vec<Diagnostic>) {
    let uri = state
        .sources
        .iter()
        .filter(|(_, v)| **v == id)
        .next()
        .map(|(k, _)| k.clone())
        .unwrap();

    match source.parse::<SourceFile>() {
        Ok(_) => (uri, Vec::new()),
        Err(err) => {
            let diagnostics = err.to_diagnostics(id);

            let mut new_diags = Vec::new();
            for diag in diagnostics {
                let diag =
                    make_lsp_diagnostic(&state.files, None, diag, |_| Ok(uri.clone())).unwrap();
                new_diags.push(diag);
            }

            (uri, new_diags)
        }
    }
}
