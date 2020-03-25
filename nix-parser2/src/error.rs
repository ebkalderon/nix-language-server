//! Common types and traits used by all kinds of errors.

use std::fmt::{self, Display, Formatter};
use std::slice::Iter;

use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use lsp_types::Diagnostic as LspDiagnostic;
use smallvec::SmallVec;

/// A trait for converting an error type into a reportable diagnostic.
///
/// This trait is generic so that both CLI diagnostics and Language Server diagnostics can be
/// produced using this interface.
pub trait ToDiagnostic<D> {
    /// Converts the error to a diagnostic `D` for the given source file specified by `file_id`.
    fn to_diagnostic(&self, file_id: FileId) -> D;
}

/// A generic growable stack for accumulating errors.
#[derive(Clone, Debug, PartialEq)]
pub struct Errors<E>(SmallVec<[E; 4]>);

impl<E> Errors<E> {
    /// Constructs a new, empty `Errors` stack.
    ///
    /// The stack will not allocate until new errors are pushed onto it.
    #[inline]
    pub fn new() -> Self {
        Errors(SmallVec::new())
    }

    /// Returns the number of errors in the stack.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the error stack is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Appends a new error to the stack.
    pub fn push(&mut self, error: E) {
        self.0.push(error);
    }

    /// Removes the last error from the stack and returns it, or [`None`] if it is empty.
    ///
    /// [`None`]: https://doc.rust-lang.org/std/option/enum.Option.html#variant.None
    pub fn pop(&mut self) -> Option<E> {
        self.0.pop()
    }

    /// Returns the last error in the stack, or [`None`] if it is empty.
    ///
    /// [`None`]: https://doc.rust-lang.org/std/option/enum.Option.html#variant.None
    pub fn last(&self) -> Option<&E> {
        self.0.last()
    }

    /// Returns an iterator of errors.
    pub fn iter(&self) -> Iter<E> {
        self.0.iter()
    }
}

impl<E> Errors<E>
where
    E: ToDiagnostic<Diagnostic<FileId>>,
{
    /// Returns an iterator which yields each error converted to a [`Diagnostic`].
    ///
    /// [`Diagnostic`]: https://docs.rs/codespan-reporting/0.9.1/codespan_reporting/diagnostic/struct.Diagnostic.html
    #[inline]
    pub fn to_diagnostics(&self, file_id: FileId) -> impl Iterator<Item = Diagnostic<FileId>> + '_ {
        self.iter().map(move |e| e.to_diagnostic(file_id))
    }
}

impl<E> Errors<E>
where
    E: ToDiagnostic<LspDiagnostic>,
{
    /// Returns an iterator which yields each error converted to an LSP [`Diagnostic`].
    ///
    /// [`Diagnostic`]: https://docs.rs/lsp-types/0.73.0/lsp_types/struct.Diagnostic.html
    #[inline]
    pub fn to_lsp_diagnostics(&self, file_id: FileId) -> impl Iterator<Item = LspDiagnostic> + '_ {
        self.iter().map(move |e| e.to_diagnostic(file_id))
    }
}

/// Helper struct for producing pretty "expected foo, found bar" error messages.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpectedFound<T, U = T> {
    expected: SmallVec<[T; 4]>,
    found: U,
}

impl<T, U> ExpectedFound<T, U>
where
    T: Display,
    U: Display,
{
    /// Creates a new `ExpectedFound` from the given list of expected items and a found item.
    pub fn new(expected: impl IntoIterator<Item = T>, found: U) -> Self {
        ExpectedFound {
            expected: expected.into_iter().collect(),
            found,
        }
    }

    /// Returns only the "expected" part of the error message string.
    ///
    /// This message is useful for diagnostic labels which annotate the source text.
    pub fn expected_message(&self) -> String {
        match self.expected.as_slice() {
            [single] => format!("expected {}", single),
            [first, second] => format!("expected {} or {}", first, second),
            [first @ .., last] => {
                let first: Vec<_> = first.iter().map(ToString::to_string).collect();
                format!("expected one of {} or {}", first.join(", "), last)
            }
            [] => "expected <eof>".to_string(),
        }
    }
}

/// Displays the complete error message string.
impl<T, U> Display for ExpectedFound<T, U>
where
    T: Display,
    U: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}, found {}", self.expected_message(), self.found)
    }
}
