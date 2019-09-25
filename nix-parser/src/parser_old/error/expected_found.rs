use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::ToDiagnostic;
use crate::ToSpan;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpectedFoundError {
    pub expected: String,
    pub found: String,
    pub span: Span,
}

impl ExpectedFoundError {
    pub fn new<T, U, S>(expected: T, found: U, span: S) -> Self
    where
        T: Into<String>,
        U: Into<String>,
        S: ToSpan,
    {
        ExpectedFoundError {
            expected: expected.into(),
            found: found.into(),
            span: span.to_span(),
        }
    }
}

impl Display for ExpectedFoundError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "expected {}, found {}", self.expected, self.found)
    }
}

impl Error for ExpectedFoundError {}

impl ToDiagnostic for ExpectedFoundError {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic {
        let label = Label::new(file, self.span, format!("expected {} here", self.expected));
        Diagnostic::new_error(self.to_string(), label)
    }
}
