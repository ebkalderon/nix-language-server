//! Common types and traits used by all kinds of errors.

use std::fmt::{self, Display, Formatter};

use codespan::FileId;
use smallvec::SmallVec;

/// A trait for converting an error type into a reportable diagnostic.
///
/// This trait is generic so that both CLI diagnostics and Language Server diagnostics can be
/// produced using this interface.
pub trait ToDiagnostic<D> {
    /// Converts the error to a diagnostic `D` for the given source file specified by `file_id`.
    fn to_diagnostic(&self, file_id: FileId) -> D;
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
