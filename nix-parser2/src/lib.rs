use codespan::Span;

pub mod ast;
pub mod error;
pub mod lexer;

/// A trait for converting a value to a `codespan::Span`.
///
/// This is helpful for getting spanned types from external crates to interoperate with `codespan`.
pub trait ToSpan {
    /// Converts the given value to a `Span`.
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
