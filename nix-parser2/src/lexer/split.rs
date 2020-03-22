//! Utilities for splitting multi-line string literals and block comments.

use nom::character::complete::multispace0;

use super::LocatedSpan;

/// Splits the input string into lines with leading indentation normalized appropriately.
///
/// This utility function is useful for splitting multi-line `''` string literals or block comments
/// in order to retrieve their actual text while preserving meaningful indentation inside of them.
///
/// # Examples
///
/// ```
/// use nix_parser2::lexer::split_lines_without_indent;
///
/// let string_literal = "
///     hello world
///
///      * bullet 1
///      * bullet 2
///
///         indented
///   deindented
///     normal
/// ";
///
/// let mut lines = split_lines_without_indent(string_literal);
/// assert_eq!(lines.next(), Some("hello world"));
/// assert_eq!(lines.next(), Some(""));
/// assert_eq!(lines.next(), Some(" * bullet 1"));
/// assert_eq!(lines.next(), Some(" * bullet 2"));
/// assert_eq!(lines.next(), Some(""));
/// assert_eq!(lines.next(), Some("    indented"));
/// assert_eq!(lines.next(), Some("deindented"));
/// assert_eq!(lines.next(), Some("normal"));
/// assert_eq!(lines.next(), Some(""));
/// assert_eq!(lines.next(), None);
/// ```
pub fn split_lines_without_indent(input: &str) -> impl Iterator<Item = &str> {
    let input = LocatedSpan::new(input);
    let (remaining, _) = multispace0::<_, (_, _)>(input).expect("multispace0 cannot fail");
    let (fragment, indent_level) = (remaining.fragment(), remaining.get_column());

    fragment.split('\n').enumerate().map(move |(i, row)| {
        if i > 0 {
            let trim_start = row
                .char_indices()
                .take_while(|(i, c)| c.is_whitespace() && *i < indent_level - 1)
                .count();
            &row[trim_start..]
        } else {
            row
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_empty_strings() {
        let lines: Vec<_> = split_lines_without_indent("").collect();
        assert_eq!(lines, vec![""]);
    }
}
