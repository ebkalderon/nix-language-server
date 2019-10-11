pub use self::expected_found::ExpectedFoundError;
pub use self::incorrect_delim::IncorrectDelimError;
pub use self::unclosed_delim::UnclosedDelimError;
pub use self::unexpected::UnexpectedError;

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::iter::FromIterator;
use std::slice::Iter;
use std::vec::IntoIter;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use nom::error::{ErrorKind, ParseError};

use crate::ToSpan;

mod expected_found;
mod incorrect_delim;
mod unclosed_delim;
mod unexpected;

pub trait ToDiagnostic {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Errors {
    errors: Vec<Error>,
}

impl Errors {
    #[inline]
    pub fn new() -> Self {
        Errors { errors: Vec::new() }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn push<E>(&mut self, error: E)
    where
        E: Into<Error>,
    {
        self.errors.push(error.into());
    }

    #[inline]
    pub fn pop(&mut self) -> Option<Error> {
        self.errors.pop()
    }

    #[inline]
    pub fn last(&mut self) -> Option<&Error> {
        self.errors.last()
    }

    #[inline]
    pub fn iter(&self) -> Iter<Error> {
        self.errors.iter()
    }

    pub fn to_diagnostics(&self, file: FileId) -> Vec<Diagnostic> {
        self.errors.iter().map(|e| e.to_diagnostic(file)).collect()
    }
}

impl Default for Errors {
    #[inline]
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

impl FromIterator<Error> for Errors {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Error>,
    {
        Errors {
            errors: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Errors {
    type Item = Error;
    type IntoIter = IntoIter<Self::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

impl<'a> IntoIterator for &'a Errors {
    type Item = &'a Error;
    type IntoIter = Iter<'a, Error>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.errors.iter()
    }
}

impl<I> ParseError<I> for Errors
where
    I: ToSpan + ToString,
{
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Errors {
            errors: vec![Error::Nom(input.to_span(), kind)],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.push(Error::Nom(input.to_span(), kind));
        other
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ExpectedFound(ExpectedFoundError),
    IncorrectDelim(IncorrectDelimError),
    UnclosedDelim(UnclosedDelimError),
    Unexpected(UnexpectedError),
    Nom(Span, ErrorKind),
    Message(Span, String),
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Error::ExpectedFound(ref e) => write!(fmt, "{}", e),
            Error::IncorrectDelim(ref e) => write!(fmt, "{}", e),
            Error::UnclosedDelim(ref e) => write!(fmt, "{}", e),
            Error::Unexpected(ref e) => write!(fmt, "{}", e),
            Error::Nom(_, ref e) => write!(fmt, "nom error: {:?}", e),
            Error::Message(_, ref e) => write!(fmt, "{}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<ExpectedFoundError> for Error {
    fn from(error: ExpectedFoundError) -> Self {
        Error::ExpectedFound(error)
    }
}

impl From<IncorrectDelimError> for Error {
    fn from(error: IncorrectDelimError) -> Self {
        Error::IncorrectDelim(error)
    }
}

impl From<UnclosedDelimError> for Error {
    fn from(error: UnclosedDelimError) -> Self {
        Error::UnclosedDelim(error)
    }
}

impl From<UnexpectedError> for Error {
    fn from(error: UnexpectedError) -> Self {
        Error::Unexpected(error)
    }
}

impl ToDiagnostic for Error {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic {
        match *self {
            Error::ExpectedFound(ref e) => e.to_diagnostic(file),
            Error::IncorrectDelim(ref e) => e.to_diagnostic(file),
            Error::Unexpected(ref e) => e.to_diagnostic(file),
            Error::UnclosedDelim(ref e) => e.to_diagnostic(file),
            Error::Nom(ref span, ref kind) => {
                let label = Label::new(file, *span, self.to_string());
                let mut diag = Diagnostic::new_bug(format!("nom error: {:?}", kind), label);
                let note = "note: this indicates an unhandled case in the parser".to_string();
                diag.notes.push(note);
                diag
            }
            Error::Message(ref span, ref msg) => {
                let label = Label::new(file, *span, msg.clone());
                Diagnostic::new_error(msg.clone(), label)
            }
        }
    }
}
