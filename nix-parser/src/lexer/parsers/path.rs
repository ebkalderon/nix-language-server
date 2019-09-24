use std::path::PathBuf;

use nom::bytes::complete::take_while1;
use nom::character::{complete::char, is_alphanumeric};
use nom::combinator::map;
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
        .captures(input.fragment)
        .or_else(|| home_path_regex().captures(input.fragment))
        .and_then(|captures| captures.get(0));

    if let Some(m) = path_match {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);

        if !span.fragment.ends_with('/') {
            let path = PathBuf::from(span.fragment);
            Ok((remaining, Token::Path(path, span.to_span())))
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

pub fn path_template(input: LocatedSpan) -> IResult<Token> {
    let name = take_while1(|c: char| is_alphanumeric(c as u8) || "/._-+".contains(c));
    let template = delimited(char('<'), name, char('>'));
    map_spanned(template, |span, text| {
        Token::PathTemplate(PathBuf::from(text.fragment), span)
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn absolute_paths() {
        let string = LocatedSpan::new("/.");
        let (_, root) = all_consuming(path)(string).unwrap();
        match root {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "/."),
            token => panic!("root path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("/foo/bar");
        let (_, simple_absolute) = all_consuming(path)(string).unwrap();
        match simple_absolute {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "/foo/bar"),
            token => panic!("simple absolute path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_absolute) = all_consuming(path)(string).unwrap();
        match complex_absolute {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "/a/b.c/d+e/F_G/h-i/123"),
            token => panic!("complex absolute path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("~/foo/bar");
        let (_, simple_home) = all_consuming(path)(string).unwrap();
        match simple_home {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "~/foo/bar"),
            token => panic!("simple home path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("~/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_home) = all_consuming(path)(string).unwrap();
        match complex_home {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "~/a/b.c/d+e/F_G/h-i/123"),
            token => panic!("complex home path produced token: {:?}", token),
        }
    }

    #[test]
    fn relative_paths() {
        let string = LocatedSpan::new("./.");
        let (_, current_dir) = all_consuming(path)(string).unwrap();
        match current_dir {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "./."),
            token => panic!("current dir relative path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("../.");
        let (_, parent_dir) = all_consuming(path)(string).unwrap();
        match parent_dir {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "../."),
            token => panic!("parent dir relative path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("./foo/bar");
        let (_, simple) = all_consuming(path)(string).unwrap();
        match simple {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "./foo/bar"),
            token => panic!("simple relative path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("./a/b.c/d+e/F_G/h-i/123");
        let (_, complex) = all_consuming(path)(string).unwrap();
        match complex {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "./a/b.c/d+e/F_G/h-i/123"),
            token => panic!("complex relative path produced token: {:?}", token),
        }

        let string = LocatedSpan::new("foo/bar");
        let (_, no_prefix) = all_consuming(path)(string).unwrap();
        match no_prefix {
            Token::Path(value, _) => assert_eq!(value.to_string_lossy(), "foo/bar"),
            token => panic!("no prefix relative path produced token: {:?}", token),
        }
    }

    #[test]
    fn path_templates() {
        let string = LocatedSpan::new("<nixpkgs>");
        let (_, nixpkgs) = all_consuming(path_template)(string).unwrap();
        match nixpkgs {
            Token::PathTemplate(value, _) => assert_eq!(value.to_string_lossy(), "nixpkgs"),
            token => panic!("nixpkgs path template produced token: {:?}", token),
        }

        let string = LocatedSpan::new("<Foo.bar-baz_quux+123>");
        let (_, complex) = all_consuming(path_template)(string).unwrap();
        match complex {
            Token::PathTemplate(value, _) => {
                assert_eq!(value.to_string_lossy(), "Foo.bar-baz_quux+123")
            }
            token => panic!("complex path template produced token: {:?}", token),
        }
    }
}
