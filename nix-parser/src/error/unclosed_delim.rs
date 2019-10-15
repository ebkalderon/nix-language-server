/// Unclosed delimiters error data structure.
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::ToDiagnostic;
use crate::ToSpan;

/// Error that occurs when at least one delimited span was left unclosed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnclosedDelimError {
    /// Locations of open delimiters that lack a matching close delimiter.
    pub unclosed_delims: Vec<Span>,
    /// Span pointing to the end of the file.
    pub eof_span: Span,
}

impl UnclosedDelimError {
    /// Constructs a new `UnclosedDelimError`.
    pub fn new<S1, S2>(delims: Vec<S1>, eof_span: S2) -> Self
    where
        S1: ToSpan,
        S2: ToSpan,
    {
        UnclosedDelimError {
            unclosed_delims: delims.into_iter().map(|span| span.to_span()).collect(),
            eof_span: eof_span.to_span(),
        }
    }
}

impl Display for UnclosedDelimError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "this file contains un-closed delimiters")
    }
}

impl Error for UnclosedDelimError {}

impl ToDiagnostic for UnclosedDelimError {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic {
        let primary = Label::new(file, self.eof_span, "expected matching delimiter here");
        let mut diagnostic = Diagnostic::new_error(self.to_string(), primary);

        for span in &self.unclosed_delims {
            let unclosed = Label::new(file, *span, "unmatched delimiter");
            diagnostic.secondary_labels.push(unclosed);
        }

        diagnostic
    }
}
