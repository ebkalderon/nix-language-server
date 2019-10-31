//! Expected/found error data structure.

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::ToDiagnostic;
use crate::ToSpan;

/// Error that occurs when an item was found, but was expecting something else.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpectedFoundError {
    /// Printable name of the item that was expected.
    pub expected: Cow<'static, str>,
    /// Printable name of the item that was found.
    pub found: Cow<'static, str>,
    /// Span of the found item.
    pub span: Span,
}

impl ExpectedFoundError {
    /// Constructs a new `ExpectedFoundError`.
    pub fn new<T, U, S>(expected: T, found: U, span: S) -> Self
    where
        T: Into<Cow<'static, str>>,
        U: Into<Cow<'static, str>>,
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
