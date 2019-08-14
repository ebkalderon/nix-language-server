use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::slice::Iter;

use codespan::ByteSpan;
use codespan_reporting::Diagnostic;
use nom::error::{ErrorKind, ParseError};

use super::{tokens, Span};
use crate::ToByteSpan;

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Errors<A> {
    pub errors: Vec<Error>,
    pub partial_ast: Option<A>,
}

impl<A> Errors<A> {
    pub fn new() -> Self {
        Errors {
            errors: Vec::new(),
            partial_ast: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn push(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn last_mut(&mut self) -> Option<&mut Error> {
        self.errors.last_mut()
    }

    pub fn iter(&self) -> Iter<Error> {
        self.errors.iter()
    }
}

impl<A> Default for Errors<A> {
    fn default() -> Self {
        Errors::new()
    }
}

impl<A> Display for Errors<A> {
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

impl<A: Debug> std::error::Error for Errors<A> {}

impl<'a, A> ParseError<Span<'a>> for Errors<A> {
    fn from_error_kind(input: Span<'a>, kind: ErrorKind) -> Self {
        let mut errors = Errors::new();
        errors.push(Error::from_nom_error(input, kind));
        errors
    }

    fn append(input: Span<'a>, kind: ErrorKind, mut other: Self) -> Self {
        other.push(Error::from_nom_error(input, kind));
        other
    }

    fn from_char(input: Span<'a>, c: char) -> Self {
        let mut errors = Errors::new();
        errors.push(Error::from_char(input, c));
        errors
    }

    fn add_context(input: Span<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.last_mut().expect("no errors").add_context(input, ctx);
        other
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    IncorrectCloseDelimiter {
        unmatched_delim: (char, ByteSpan),
        candidate_span: Option<ByteSpan>,
        unclosed_span: Option<ByteSpan>,
    },
    UnexpectedToken {
        token: String,
        span: ByteSpan,
        expected: Vec<String>,
    },
}

impl Error {
    fn from_nom_error<'a>(input: Span<'a>, kind: ErrorKind) -> Self {
        println!("received input: \"{}\"", input.fragment);
        println!(" error kind is: {:?}", kind);
        match kind {
            ErrorKind::Verify => Error::UnexpectedToken {
                token: input.fragment.to_string(),
                span: input.to_byte_span(),
                expected: Vec::new(),
            },
            _ => Error::UnexpectedToken {
                token: "fake error".into(),
                span: ByteSpan::new(0.into(), 0.into()),
                expected: Vec::new(),
            },
        }
    }

    fn from_char<'a>(input: Span<'a>, c: char) -> Self {
        Error::UnexpectedToken {
            token: input.fragment.chars().next().unwrap_or('a').to_string(),
            span: input.to_byte_span(),
            expected: vec![c.to_string()],
        }
    }

    fn add_context<'a>(&mut self, input: Span<'a>, ctx: &'static str) {
        match *self {
            Error::UnexpectedToken { ref mut token, .. } => *token = ctx.to_owned(),
            _ => {}
        }
    }
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        use Error::*;
        match *self {
            IncorrectCloseDelimiter {
                unmatched_delim: (delim, _),
                ..
            } => write!(fmt, "incorrect close delimiter: `{}`", delim),
            UnexpectedToken {
                ref token,
                ref expected,
                ..
            } => {
                let mut expected: Vec<_> = expected.iter().map(|t| format!("`{}`", t)).collect();
                if expected.is_empty() {
                    write!(fmt, "found unexpected token `{}`", token)
                } else if expected.len() == 2 {
                    let first = expected.remove(0);
                    let second = expected.remove(1);
                    write!(fmt, "expected {} or {}, found `{}`", first, second, token)
                } else {
                    write!(fmt, "expected {}, found `{}`", expected.join(", "), token)
                }
            }
        }
    }
}

impl std::error::Error for Error {}

impl ToDiagnostic for Error {
    fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            _ => unimplemented!(),
        }
    }
}
