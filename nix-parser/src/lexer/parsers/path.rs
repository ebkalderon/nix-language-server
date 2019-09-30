use std::path::PathBuf;

use nom::bytes::complete::take_while1;
use nom::character::complete::char;
use nom::sequence::delimited;
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use crate::error::{Error, Errors};
use crate::lexer::util::map_spanned;
use crate::lexer::{IResult, LocatedSpan, Token};
use crate::ToSpan;

pub fn path(input: LocatedSpan) -> IResult<Token> {
    let path_match = normal_path_regex()
        .find(input.fragment)
        .or_else(|| home_path_regex().find(input.fragment));

    if let Some(m) = path_match {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);

        if !span.fragment.ends_with('/') {
            let path = PathBuf::from(span.fragment);
            Ok((remaining, Token::Path(path, span.to_span())))
        } else {
            let message = "paths cannot have trailing slashes".to_string();
            let error = Error::Message(span.to_span(), message);
            let token = Token::Unknown(span.to_string(), span.to_span(), error);
            Ok((remaining, token))
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

pub fn path_template(input: LocatedSpan) -> IResult<Token> {
    let name = take_while1(|c: char| c.is_alphanumeric() || "/._-+".contains(c));
    let template = delimited(char('<'), name, char('>'));
    map_spanned(template, |span, text| {
        Token::PathTemplate(PathBuf::from(text.fragment), span)
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    fn assert_path_eq(string: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(path)(span) {
            Ok((_, Token::Path(value, _))) => assert_eq!(value, PathBuf::from(string)),
            Ok((_, token)) => panic!("parsing path {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing path {:?} failed: {:?}", string, err),
        }
    }

    fn assert_path_template_eq(string: &str, expected: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(path_template)(span) {
            Ok((_, Token::PathTemplate(value, _))) => assert_eq!(value, PathBuf::from(expected)),
            Ok((_, token)) => panic!("parsing template {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing template {:?} failed: {:?}", string, err),
        }
    }

    #[test]
    fn absolute_paths() {
        assert_path_eq("/.");
        assert_path_eq("/foo/bar");
        assert_path_eq("/a/b.c/d+e/F_G/h-i/123");
        assert_path_eq("~/foo/bar");
        assert_path_eq("~/a/b.c/d+e/F_G/h-i/123");
    }

    #[test]
    fn relative_paths() {
        assert_path_eq("./.");
        assert_path_eq("../.");
        assert_path_eq("./foo/bar");
        assert_path_eq("./a/b.c/d+e/F_G/h-i/123");
        assert_path_eq("foo/bar");
    }

    #[test]
    fn path_templates() {
        assert_path_template_eq("<nixpkgs>", "nixpkgs");
        assert_path_template_eq("<Foo.bar-baz_quux+123>", "Foo.bar-baz_quux+123");
    }
}
