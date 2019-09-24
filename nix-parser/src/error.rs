pub use self::expected_found::ExpectedFoundError;
pub use self::incorrect_delim::IncorrectDelimError;
pub use self::unexpected::UnexpectedError;

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::slice::Iter;
use std::vec::IntoIter;

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use nom::error::{ErrorKind, ParseError};
use nom::IResult;

use crate::ToSpan;

mod expected_found;
mod incorrect_delim;
mod unexpected;

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

    pub fn pop(&mut self) -> Option<Error> {
        self.errors.pop()
    }

    pub fn last(&mut self) -> Option<&Error> {
        self.errors.last()
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

impl<I> ParseError<I> for Errors
where
    I: ToSpan + ToString,
{
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        let mut errors = Errors::new();
        errors.push(Error::Nom(input.to_span(), input.to_string(), kind));
        errors
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.push(Error::Nom(input.to_span(), input.to_string(), kind));
        other
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    ExpectedFound(ExpectedFoundError),
    IncorrectDelim(IncorrectDelimError),
    Unexpected(UnexpectedError),
    Nom(Span, String, ErrorKind),
    Message(Span, String),
}

impl Error {
    pub fn as_nom_error(&self) -> Option<(Span, &str, ErrorKind)> {
        match *self {
            Error::Nom(ref span, ref frag, ref kind) => Some((*span, frag, *kind)),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Error::ExpectedFound(ref e) => write!(fmt, "{}", e),
            Error::IncorrectDelim(ref e) => write!(fmt, "{}", e),
            Error::Unexpected(ref e) => write!(fmt, "{}", e),
            Error::Nom(_, ref frag, ref e) => write!(fmt, "nom error: (\"{}\", {:?})", frag, e),
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
            Error::Nom(ref span, ref _frag, ref kind) => {
                let label = Label::new(file, *span, self.to_string());
                Diagnostic::new_error(format!("nom error: {:?}", kind), label)
            }
            Error::Message(ref span, ref msg) => {
                let label = Label::new(file, *span, msg.clone());
                Diagnostic::new_error(msg.clone(), label)
            }
        }
    }
}

/// Combinator which converts an underlying `nom` error, if any, into a custom parse error.
pub fn map_err<I, O, P, F, E>(parser: P, op: F) -> impl Fn(I) -> IResult<I, O, Errors>
where
    P: Fn(I) -> IResult<I, O, Errors>,
    F: Fn(Span, &str, ErrorKind) -> E,
    E: Into<Error>,
{
    move |input| match parser(input) {
        Ok(output) => Ok(output),
        Err(nom::Err::Error(mut err)) => {
            if let Some((span, frag, kind)) = err.last().and_then(|e| e.as_nom_error()) {
                let new_err = op(span, frag, kind);
                err.pop();
                err.push(new_err);
            }
            Err(nom::Err::Error(err))
        }
        Err(nom::Err::Failure(mut err)) => {
            if let Some((span, frag, kind)) = err.last().and_then(|e| e.as_nom_error()) {
                let new_err = op(span, frag, kind);
                err.pop();
                err.push(new_err);
            }
            Err(nom::Err::Failure(err))
        }
        Err(err) => Err(err),
    }
}
