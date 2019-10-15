#![forbid(unsafe_code)]
#![recursion_limit = "128"]

use codespan::Span;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

pub trait HasSpan {
    fn span(&self) -> Span;
}

impl HasSpan for Span {
    fn span(&self) -> Span {
        *self
    }
}

pub trait ToSpan {
    fn to_span(&self) -> Span;
}

impl ToSpan for Span {
    fn to_span(&self) -> Span {
        *self
    }
}

impl<'a, T: ToSpan> ToSpan for &'a T {
    fn to_span(&self) -> Span {
        (*self).to_span()
    }
}
