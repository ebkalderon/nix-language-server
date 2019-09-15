pub use self::close_delim::CloseDelimiterError;
pub use self::expected_found::ExpectedFoundError;

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::slice::Iter;
use std::vec::IntoIter;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use nom::error::{ErrorKind, ParseError};
use nom::Slice;

use super::LocatedSpan;
use crate::ToSpan;

mod close_delim;
mod expected_found;

pub trait ToDiagnostic {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Errors {
    errors: Vec<Error>,
}

impl Errors {
    pub fn new() -> Self {
        Errors { errors: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn push<E>(&mut self, error: E)
    where
        E: Into<Error>,
    {
        self.errors.push(error.into());
    }

    pub fn iter(&self) -> Iter<Error> {
        self.errors.iter()
    }

    pub fn to_diagnostics(&self, file: FileId) -> Vec<Diagnostic> {
        self.errors.iter().map(|e| e.to_diagnostic(file)).collect()
    }
}

impl Default for Errors {
    fn default() -> Self {
        Errors::new()
    }
}

impl Display for Errors {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let errors: Vec<_> = self
            .errors
            .iter()
            .enumerate()
            .map(|(i, e)| format!("{}: {}", i, e))
            .collect();
        write!(fmt, "{}", errors.join("\n"))
    }
}

impl std::error::Error for Errors {}

impl Extend<Error> for Errors {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Error>,
    {
        self.errors.extend(iter);
    }
}

impl IntoIterator for Errors {
    type Item = Error;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

impl<'a> IntoIterator for &'a Errors {
    type Item = &'a Error;
    type IntoIter = Iter<'a, Error>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.iter()
    }
}

impl<'a> ParseError<LocatedSpan<'a>> for Errors {
    fn from_error_kind(input: LocatedSpan<'a>, kind: ErrorKind) -> Self {
        let mut errors = Errors::new();
        errors.push(Error::Message(
            input.slice(0..0).to_span(),
            format!("nom error: {:?}", kind),
        ));
        errors
    }

    fn append(input: LocatedSpan<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.push(Error::Message(
            input.to_span(),
            format!("nom error: {:?}", kind),
        ));
        other
    }

    fn from_char(input: LocatedSpan<'a>, c: char) -> Self {
        let span = input.slice(0..0).to_span();
        let expected = vec![format!("`{}`", c)];
        let found = input
            .fragment
            .chars()
            .next()
            .map(|c| format!("`{}`", c))
            .unwrap_or_else(|| "EOF".to_string());

        let mut errors = Errors::new();
        errors.push(ExpectedFoundError::new(expected, found, span));
        errors
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    CloseDelimiter(CloseDelimiterError),
    ExpectedFound(ExpectedFoundError),
    Message(Span, String),
}

impl Error {
    pub fn expected_found(expected: Vec<String>, found: String, span: Span) -> Self {
        Error::ExpectedFound(ExpectedFoundError {
            expected,
            found,
            span,
        })
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Error::CloseDelimiter(ref e) => write!(fmt, "{}", e),
            Error::ExpectedFound(ref e) => write!(fmt, "{}", e),
            Error::Message(_, ref e) => write!(fmt, "{}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<CloseDelimiterError> for Error {
    fn from(error: CloseDelimiterError) -> Self {
        Error::CloseDelimiter(error)
    }
}

impl From<ExpectedFoundError> for Error {
    fn from(error: ExpectedFoundError) -> Self {
        Error::ExpectedFound(error)
    }
}

impl ToDiagnostic for Error {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic {
        match *self {
            Error::CloseDelimiter(ref e) => e.to_diagnostic(file),
            Error::ExpectedFound(ref e) => e.to_diagnostic(file),
            Error::Message(ref span, ref msg) => {
                let label = Label::new(file, *span, msg.clone());
                Diagnostic::new_bug(msg.clone(), label)
            }
        }
    }
}
