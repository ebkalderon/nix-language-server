pub mod ast;
pub mod error;

/// A trait for converting a value to a `codespan::Span`.
///
/// This is helpful for getting spanned types from external crates to interoperate with `codespan`.
pub trait ToSpan {
    /// Converts the given value to a `Span`.
    fn to_span(&self) -> Span;
}
