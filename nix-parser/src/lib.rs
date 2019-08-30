#![forbid(unsafe_code)]

use codespan::Span;

pub mod ast;
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
