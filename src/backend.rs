use std::collections::HashMap;
use std::sync::Mutex;

use codespan::{FileId, Files};
use futures::future::{self, FutureResult};
use jsonrpc_core::types::params::Params;
use jsonrpc_core::{Error as RpcError, Result as RpcResult};
use log::{debug, error, info};
use lsp_types::*;
use nix_parser::ast::SourceFile;

use crate::server::LanguageServer;
use crate::Error;

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
                let source = get_or_insert_source(&mut state, &params.text_document);
                match source.parse::<SourceFile>() {
                    Ok(file) => info!("parsed source file: {:?}", file),
                    Err(err) => error!("failed to parse source file: {:?}", err),
                }
            }
        }
    }

    fn did_save(&self, params: Params) {
        info!("did_save: {:?}", params);
    }

    fn did_change(&self, params: Params) {
        match params.parse::<DidChangeTextDocumentParams>() {
            Err(err) => error!("{}", err),
            Ok(params) => {
                let mut state = self.state.lock().unwrap_or_else(|e| e.into_inner());
                info!("did_change: {:?}", params);
                let source = reload_source(
                    &mut state,
                    &params.text_document,
                    &params.content_changes[0].text,
                );
                match source.parse::<SourceFile>() {
                    Ok(file) => info!("parsed source file: {:?}", file),
                    Err(err) => error!("failed to parse source file: {:?}", err),
                }
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

fn reload_source<'a>(
    state: &'a mut State,
    document: &VersionedTextDocumentIdentifier,
    text: &'a str,
) -> &'a str {
    if let Some(id) = state.sources.get(&document.uri) {
        state.files.update(*id, text);
        state.files.source(*id)
    } else {
        let id = state.files.add(document.uri.to_string(), text);
        state.sources.insert(document.uri.clone(), id);
        state.files.source(id)
    }
}

fn get_or_insert_source<'a>(state: &'a mut State, document: &TextDocumentItem) -> &'a str {
    if let Some(id) = state.sources.get(&document.uri) {
        state.files.source(*id)
    } else {
        let id = state
            .files
            .add(document.uri.to_string(), document.text.clone());
        state.sources.insert(document.uri.clone(), id);
        state.files.source(id)
    }
}
