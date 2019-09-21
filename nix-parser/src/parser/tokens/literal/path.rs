use std::path::PathBuf;

use nom::bytes::complete::take_while1;
use nom::character::{complete::char, is_alphanumeric};
use nom::combinator::{cut, map};
use nom::sequence::delimited;
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use crate::parser::error::{Error, Errors};
use crate::parser::{IResult, LocatedSpan};
use crate::ToSpan;

pub fn path(input: LocatedSpan) -> IResult<PathBuf> {
    let path_match = normal_path_regex()
        .captures(input.fragment)
        .or_else(|| home_path_regex().captures(input.fragment))
        .and_then(|captures| captures.get(0));

    if let Some(m) = path_match {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);

        if !span.fragment.ends_with('/') {
            Ok((remaining, PathBuf::from(span.fragment)))
        } else {
            let mut errors = Errors::new();
            let message = format!("paths cannot have trailing slashes");
            errors.push(Error::Message(span.to_span(), message));
            Err(nom::Err::Failure(errors))
        }
    } else {
        Err(nom::Err::Error(Errors::new()))
    }
}

fn normal_path_regex<'a>() -> &'a Regex {
    static PATH: OnceCell<Regex> = OnceCell::new();
    PATH.get_or_init(|| Regex::new(r#"\A[a-zA-Z0-9\._\-\+]*(/[a-zA-Z0-9\._\-\+]+)+/?"#).unwrap())
}

fn home_path_regex<'a>() -> &'a Regex {
    static PATH: OnceCell<Regex> = OnceCell::new();
    PATH.get_or_init(|| Regex::new(r#"\A\~(/[a-zA-Z0-9\._\-\+]+)+/?"#).unwrap())
}

pub fn path_template(input: LocatedSpan) -> IResult<String> {
    let name = take_while1(|c: char| is_alphanumeric(c as u8) || "/._-+".contains(c));
    let template = delimited(char('<'), name, cut(char('>')));
    map(template, |tmp: LocatedSpan| tmp.fragment.to_string())(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn absolute_paths() {
        let string = LocatedSpan::new("/.");
        let (_, root) = all_consuming(path)(string).unwrap();
        assert_eq!(root.to_string_lossy(), "/.");

        let string = LocatedSpan::new("/foo/bar");
        let (_, simple_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_absolute.to_string_lossy(), "/foo/bar");

        let string = LocatedSpan::new("/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_absolute.to_string_lossy(), "/a/b.c/d+e/F_G/h-i/123");

        let string = LocatedSpan::new("~/foo/bar");
        let (_, simple_home) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_home.to_string_lossy(), "~/foo/bar");

        let string = LocatedSpan::new("~/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_home) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_home.to_string_lossy(), "~/a/b.c/d+e/F_G/h-i/123");
    }

    #[test]
    fn relative_paths() {
        let string = LocatedSpan::new("./.");
        let (_, current_dir) = all_consuming(path)(string).unwrap();
        assert_eq!(current_dir.to_string_lossy(), "./.");

        let string = LocatedSpan::new("../.");
        let (_, parent_dir) = all_consuming(path)(string).unwrap();
        assert_eq!(parent_dir.to_string_lossy(), "../.");

        let string = LocatedSpan::new("./foo/bar");
        let (_, simple) = all_consuming(path)(string).unwrap();
        assert_eq!(simple.to_string_lossy(), "./foo/bar");

        let string = LocatedSpan::new("./a/b.c/d+e/F_G/h-i/123");
        let (_, complex) = all_consuming(path)(string).unwrap();
        assert_eq!(complex.to_string_lossy(), "./a/b.c/d+e/F_G/h-i/123");

        let string = LocatedSpan::new("foo/bar");
        let (_, no_prefix) = all_consuming(path)(string).unwrap();
        assert_eq!(no_prefix.to_string_lossy(), "foo/bar");
    }

    #[test]
    fn path_templates() {
        let string = LocatedSpan::new("<nixpkgs>");
        let (_, nixpkgs) = all_consuming(path_template)(string).unwrap();
        assert_eq!(nixpkgs, "nixpkgs");

        let string = LocatedSpan::new("<Foo.bar-baz_quux+123>");
        let (_, complex) = all_consuming(path_template)(string).unwrap();
        assert_eq!(complex, "Foo.bar-baz_quux+123");
    }
}
