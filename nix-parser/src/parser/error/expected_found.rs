use std::error::Error;
use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::ToDiagnostic;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpectedFoundError {
    pub expected: Vec<String>,
    pub found: String,
    pub span: Span,
}

impl ExpectedFoundError {
    fn expected_message(&self) -> String {
        let expected: String = self.expected.join(", ");
        if self.expected.len() == 1 {
            format!("expected {}", expected)
        } else if self.expected.len() == 2 {
            let first = &self.expected[0];
            let second = &self.expected[1];
            format!("expected one of {} or {}", first, second)
        } else {
            format!("expected one of {}", expected)
        }
    }
}

impl Display for ExpectedFoundError {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}, found {}", self.expected_message(), self.found)
    }
}

impl Error for ExpectedFoundError {}

impl ToDiagnostic for ExpectedFoundError {
    fn to_diagnostic(&self, file: FileId) -> Diagnostic {
        let label = Label::new(file, self.span, format!("{} here", self.expected_message()));
        Diagnostic::new_error(self.to_string(), label)
    }
}
