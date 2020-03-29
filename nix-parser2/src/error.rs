//! Error types for lexing and parsing.

use codespan::{FileId, Files, Span};
use codespan_lsp::byte_span_to_range;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lsp_types::Diagnostic as LspDiagnostic;
use nix_errors::{LspResult, ToDiagnostic};
use nix_lexer::TokenKind;

/// A list of all valid errors that can be emitted by the parser.
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    /// A path or path template literal contains a trailing slash.
    TrailingSlash(Span, TokenKind),
    /// A block comment or string literal was left unterminated.
    Unterminated(Span, TokenKind),
}

impl ToDiagnostic<Diagnostic<FileId>> for Error {
    /// Converts this error to a `codespan_reporting::diagnostic::Diagnostic`.
    fn to_diagnostic<S: AsRef<str>>(&self, _: &Files<S>, file_id: FileId) -> Diagnostic<FileId> {
        match *self {
            Error::TrailingSlash(ref span, ref kind) => {
                let message = format!("{}s cannot have a trailing slash", kind.description());
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![Label::primary(file_id, *span)])
            }
            Error::Unterminated(ref span, ref kind) => {
                let message = format!("unterminated {}", kind.description());
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![Label::primary(file_id, *span)])
            }
        }
    }
}

impl ToDiagnostic<LspResult<LspDiagnostic>> for Error {
    /// Converts this error to a `Result<lsp_types::Diagnostic, codespan_lsp::Error>`.
    fn to_diagnostic<S: AsRef<str>>(
        &self,
        files: &Files<S>,
        file_id: FileId,
    ) -> LspResult<LspDiagnostic> {
        match *self {
            Error::TrailingSlash(ref span, ref kind) => {
                let message = format!("{}s cannot have a trailing slash", kind.description());
                byte_span_to_range(files, file_id, *span)
                    .map(|range| LspDiagnostic::new_simple(range, message))
            }
            Error::Unterminated(ref span, ref kind) => {
                let message = format!("unterminated {}", kind.description());
                byte_span_to_range(files, file_id, *span)
                    .map(|range| LspDiagnostic::new_simple(range, message))
            }
        }
    }
}
