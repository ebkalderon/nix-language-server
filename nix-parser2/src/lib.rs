use codespan::Span;

pub mod error;
pub mod lexer;

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
