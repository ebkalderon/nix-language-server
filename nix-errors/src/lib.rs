//! Common types and traits used by all kinds of errors.

use std::fmt::{self, Display, Formatter};
use std::iter::FromIterator;
use std::slice::Iter;

use codespan::{FileId, Files};
use codespan_reporting::diagnostic::Diagnostic;
use lsp_types::Diagnostic as LspDiagnostic;
use smallvec::SmallVec;

/// Number of errors to keep on the stack before spilling onto the heap.
const NUM_ERRORS: usize = 4;

/// A specialized `Result` type for LSP diagnostic conversions.
pub type LspResult<T> = std::result::Result<T, codespan_lsp::Error>;

/// A trait for converting an error type into a reportable diagnostic.
///
/// This trait is generic so that both CLI diagnostics and Language Server diagnostics can be
/// produced using this interface.
pub trait ToDiagnostic<D> {
    /// Converts the error to a diagnostic `D` for the given source file specified by `file_id`.
    fn to_diagnostic<S: AsRef<str>>(&self, files: &Files<S>, file_id: FileId) -> D;
}

/// A generic growable stack for accumulating errors.
#[derive(Clone, Debug, PartialEq)]
pub struct Errors<E>(SmallVec<[E; NUM_ERRORS]>);

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
    pub fn to_diagnostics<'a, S>(
        &'a self,
        files: &'a Files<S>,
        file_id: FileId,
    ) -> impl Iterator<Item = Diagnostic<FileId>> + 'a
    where
        S: AsRef<str>,
    {
        self.iter().map(move |e| e.to_diagnostic(files, file_id))
    }
}

impl<E> Errors<E>
where
    E: ToDiagnostic<LspResult<LspDiagnostic>>,
{
    /// Returns an iterator which yields each error converted to an LSP [`Diagnostic`].
    ///
    /// [`Diagnostic`]: https://docs.rs/lsp-types/0.73.0/lsp_types/struct.Diagnostic.html
    pub fn to_lsp_diagnostics<'a, S>(
        &'a self,
        files: &'a Files<S>,
        file_id: FileId,
    ) -> impl Iterator<Item = LspResult<LspDiagnostic>> + 'a
    where
        S: AsRef<str>,
    {
        self.iter().map(move |e| e.to_diagnostic(files, file_id))
    }
}

impl<E> Default for Errors<E> {
    /// Creates an empty `Errors` stack.
    fn default() -> Self {
        Errors::new()
    }
}

impl<E> Extend<E> for Errors<E> {
    fn extend<I: IntoIterator<Item = E>>(&mut self, iter: I) {
        self.0.extend(iter)
    }
}

impl<E> FromIterator<E> for Errors<E> {
    fn from_iter<I: IntoIterator<Item = E>>(iter: I) -> Self {
        Errors(SmallVec::from_iter(iter))
    }
}

impl<E> IntoIterator for Errors<E> {
    type Item = E;
    type IntoIter = smallvec::IntoIter<[E; NUM_ERRORS]>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, E> IntoIterator for &'a Errors<E> {
    type Item = &'a E;
    type IntoIter = Iter<'a, E>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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

/// A partial value which might contain errors.
///
/// `Partial<T, E>` is a tuple-like data structure which contains some value and an associated
/// [`Errors`] stack.
///
/// [`Errors`]: ../struct.Errors.html
///
/// This type is used to accumulate errors and apply monadic transformations to possibly incomplete
/// or invalid data structures. Consumers of `Partial<T, E>` values can choose to assert that the
/// contained value exists without errors with [`Partial::verify()`], which will consume the
/// `Partial<T, E>` and transform it into a `Result<T, Errors<E>>` which can be handled normally.
///
/// [`Partial::verify()`]: #method.verify
#[must_use = "partial values must be either verified or destructured"]
#[derive(Clone, Debug, PartialEq)]
pub struct Partial<T, E> {
    value: T,
    errors: Errors<E>,
}

impl<T, E> Partial<T, E> {
    /// Constructs a new `Partial<T, E>` with the given initial value.
    pub fn new(value: T) -> Self {
        Partial::with_errors(value, Errors::new())
    }

    /// Constructs a new `Partial<T, E>` with the given initial value and a stack of errors.
    pub fn with_errors(value: T, errors: Errors<E>) -> Self {
        Partial { value, errors }
    }

    /// Returns whether this partial value contains errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Returns the contained partial value, if any.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Returns a reference to any errors associated with the partial value.
    pub fn errors(&self) -> &Errors<E> {
        &self.errors
    }

    /// Appends the given error to the error stack contained in this partial value.
    pub fn extend_errors<I: IntoIterator<Item = E>>(&mut self, error: I) {
        self.errors.extend(error);
    }

    /// Calls `f` on the contained value and accumulates any errors it may have produced.
    pub fn flat_map<U, F>(mut self, f: F) -> Partial<U, E>
    where
        F: FnOnce(T) -> Partial<U, E>,
    {
        let mut partial = f(self.value);
        self.errors.extend(partial.errors);
        partial.errors = self.errors;
        partial
    }

    /// Destructures this `Partial<T, E>` into a tuple of its inner components.
    pub fn into_inner(self) -> (T, Errors<E>) {
        (self.value, self.errors)
    }

    /// Transforms the `Partial<T, E>` into a `Result<T, Errors<E>>`, asserting that the contained
    /// value has no errors.
    pub fn verify(self) -> Result<T, Errors<E>> {
        if self.errors.is_empty() {
            Ok(self.value)
        } else {
            Err(self.errors)
        }
    }
}
