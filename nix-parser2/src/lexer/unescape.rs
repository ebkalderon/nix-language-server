//! Utilities for validating string literals and turning them into the values that they represent.

use std::borrow::Cow;

use super::tokens::StringKind;

/// Takes the contents of a string literal (without quotes) and returns an unescaped string.
///
/// # Examples
///
/// Normal strings:
///
/// ```
/// use nix_parser2::lexer::{unescape_str, StringKind};
///
/// let unescaped = unescape_str("foo \\${bar} \\n\\r\\t \\' \\^ baz", StringKind::Normal);
/// assert_eq!(unescaped, "foo ${bar} \n\r\t ' ^ baz");
/// ```
///
/// Indented strings:
///
/// ```
/// use nix_parser2::lexer::{unescape_str, StringKind};
///
/// let unescaped = unescape_str("foo ''${bar} ''' ''^ baz", StringKind::Indented);
/// assert_eq!(unescaped, "foo ${bar} ' ''^ baz");
/// ```
pub fn unescape_str(s: &str, kind: StringKind) -> Cow<str> {
    match kind {
        StringKind::Normal => unescape_normal_str(s),
        StringKind::Indented => unescape_indented_str(s),
    }
}

fn unescape_normal_str(s: &str) -> Cow<str> {
    if s.contains('\\') {
        let mut res = String::with_capacity(s.len());
        let mut chars = s.chars();

        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => res.push('\n'),
                    Some('r') => res.push('\r'),
                    Some('t') => res.push('\t'),
                    Some(c) => res.push(c),
                    None => (),
                }
            } else {
                res.push(c);
            }
        }

        Cow::Owned(res)
    } else {
        Cow::Borrowed(s)
    }
}

fn unescape_indented_str(s: &str) -> Cow<str> {
    if s.contains("''") {
        let mut res = String::with_capacity(s.len());
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\'' && chars.peek() == Some(&'\'') {
                match chars.by_ref().skip(1).next() {
                    Some('$') => res.push('$'),
                    Some('\'') => res.push('\''),
                    Some(c) => {
                        res.push('\'');
                        res.push('\'');
                        res.push(c);
                    }
                    None => res.push('\''),
                }
            } else {
                res.push(c);
            }
        }

        Cow::Owned(res)
    } else {
        Cow::Borrowed(s)
    }
}
