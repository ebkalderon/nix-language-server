//! Combinators for lexing single paths and path templates.

use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::char;
use nom::combinator::{map, opt, recognize};
use nom::multi::many1;
use nom::sequence::{delimited, pair};

use crate::error::Error;
use crate::lexer::util::map_spanned;
use crate::lexer::{IResult, LocatedSpan, Token};
use crate::ToSpan;

pub fn path(input: LocatedSpan) -> IResult<Token> {
    let segments = many1(pair(char('/'), take_while1(path_segment)));
    let normal = map(pair(take_while(path_segment), segments), |_| ());
    let segments = many1(pair(char('/'), take_while1(path_segment)));
    let home = map(pair(char('~'), segments), |_| ());
    let (remaining, path) = recognize(pair(alt((normal, home)), opt(char('/'))))(input)?;

    if !path.fragment.ends_with('/') {
        Ok((remaining, Token::Path(path.fragment.into(), path.to_span())))
    } else {
        let message = "paths cannot have trailing slashes".into();
        let error = Error::Message(path.to_span(), message);
        let token = Token::Unknown(path.fragment.into(), path.to_span(), error);
        Ok((remaining, token))
    }
}

fn path_segment(c: char) -> bool {
    c.is_alphanumeric() || c == '.' || c == '_' || c == '-' || c == '+'
}

pub fn path_template(input: LocatedSpan) -> IResult<Token> {
    let name = take_while1(|c: char| c.is_alphanumeric() || "/._-+".contains(c));
    let template = delimited(char('<'), name, char('>'));
    map_spanned(template, |span, text| {
        Token::PathTemplate(text.fragment.into(), span)
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    fn assert_path_eq(string: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(path)(span) {
            Ok((_, Token::Path(value, _))) => assert_eq!(value, string),
            Ok((_, token)) => panic!("parsing path {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing path {:?} failed: {:?}", string, err),
        }
    }

    fn assert_path_template_eq(string: &str, expected: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(path_template)(span) {
            Ok((_, Token::PathTemplate(value, _))) => assert_eq!(value, expected),
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
