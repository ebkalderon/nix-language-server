//! Extension traits and types for `std::fmt`.

use std::fmt::{Formatter, Result as FmtResult, Write};

/// A trait which extends the functionality of [`std::fmt::Formatter`].
///
/// [`std::fmt::Formatter`]: https://doc.rust-lang.org/std/fmt/struct.Formatter.html
pub trait FormatterExt<'a> {
    /// Indents the given formatter with the given number of spaces.
    fn indent<'b>(&'b mut self, level: usize) -> Indented<'b, 'a>;
}

impl<'a> FormatterExt<'a> for Formatter<'a> {
    fn indent<'b>(&'b mut self, level: usize) -> Indented<'b, 'a> {
        Indented {
            fmt: self,
            level,
            newline: true,
        }
    }
}

/// Formatter which indents each line by a certain amount.
#[allow(missing_debug_implementations)]
pub struct Indented<'a, 'b: 'a> {
    fmt: &'a mut Formatter<'b>,
    level: usize,
    newline: bool,
}

impl<'a, 'b: 'a> Write for Indented<'a, 'b> {
    fn write_str(&mut self, s: &str) -> FmtResult {
        for c in s.chars() {
            if c == '\n' {
                self.fmt.write_char(c)?;
                self.newline = true;
                continue;
            }

            if self.newline {
                write!(self.fmt, "{:indent$}", "", indent = self.level)?;
            }

            self.fmt.write_char(c)?;
            self.newline = false;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use super::*;

    #[derive(Debug)]
    struct Indented<T: Display>(T);

    impl<T: Display> Display for Indented<T> {
        fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
            write!(fmt.indent(2), "{}", self.0)
        }
    }

    #[test]
    fn display_with_indentation() {
        assert_eq!("  one", Indented("one").to_string());
        assert_eq!("    two", Indented(Indented("two")).to_string());
        assert_eq!("  multi\n  line\n", Indented("multi\nline\n").to_string());
    }
}
