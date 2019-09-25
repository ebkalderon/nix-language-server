pub use self::keywords::*;
pub use self::literal::literal;
pub use self::ops::*;
pub use self::strings::single_quote_string;

use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_until, take_while};
use nom::character::complete::{
    char, line_ending, multispace0, multispace1, not_line_ending, space0,
};
use nom::combinator::{cut, map, recognize};
use nom::multi::{many0, many1, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use super::error::{Errors, ExpectedFoundError};
use super::{map_spanned, IResult, LocatedSpan};
use crate::ast::tokens::{Comment, Ident, IdentPath};

mod keywords;
mod literal;
mod ops;
mod strings;

pub fn comment(input: LocatedSpan) -> IResult<Comment> {
    let span = map(not_line_ending, |s: LocatedSpan| s.fragment);
    let rows = separated_nonempty_list(pair(line_ending, space0), preceded(char('#'), span));
    let line = map(rows, |r| r.join("\n"));

    let span = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    let rows = map(span, |s: LocatedSpan| s.fragment.lines());
    let block = map(rows, |r| r.collect::<Vec<_>>().join("\n"));

    let comment = alt((line, block));
    map_spanned(comment, |span, c| Comment::from((c, span)))(input)
}

pub fn space(input: LocatedSpan) -> IResult<()> {
    let line = delimited(char('#'), not_line_ending, line_ending);
    let block = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    map(many0(alt((multispace1, line, block))), |_| ())(input)
}

pub fn space1(input: LocatedSpan) -> IResult<()> {
    let line = delimited(char('#'), not_line_ending, line_ending);
    let block = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    map(many1(alt((multispace1, line, block))), |_| ())(input)
}

pub fn space_until_final_comment(input: LocatedSpan) -> IResult<()> {
    let trimmed_comment = terminated(recognize(comment), multispace0);
    let (remainder, comments) = preceded(multispace0, many0(trimmed_comment))(input)?;

    let chars_to_take = comments
        .last()
        .map(|comment| comment.offset - input.offset)
        .unwrap_or_else(|| remainder.offset - input.offset);

    map(take(chars_to_take), |_| ())(input)
}

pub fn identifier(input: LocatedSpan) -> IResult<Ident> {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| Regex::new(r#"\A[a-zA-Z_][a-zA-Z0-9_'\-]*"#).unwrap());

    if let Some(m) = regex.captures(input.fragment).and_then(|c| c.get(0)) {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);

        if !is_keyword(&span) {
            Ok((remaining, Ident::from((span.fragment, span))))
        } else {
            let mut errors = Errors::new();
            errors.push(ExpectedFoundError::new(
                "identifier",
                format!("keyword (`{}`)", span.fragment),
                span,
            ));
            Err(nom::Err::Error(errors))
        }
    } else {
        let (_, token) = take_while(|c: char| !" \n,;=)}".contains(c))(input)?;
        let mut errors = Errors::new();
        errors.push(ExpectedFoundError::new(
            "identifier",
            format!("`{}`", token.fragment),
            token,
        ));
        Err(nom::Err::Error(errors))
    }
}

pub fn ident_path(input: LocatedSpan) -> IResult<IdentPath> {
    let path = separated_nonempty_list(char('.'), identifier);
    map_spanned(path, |span, idents| IdentPath::from((idents, span)))(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;
    use crate::nix_token;

    #[test]
    fn comments() {
        let string = LocatedSpan::new("# hello world\n    #    long indent");
        let (_, single_line) = all_consuming(comment)(string).expect("single-line comment failed");
        assert_eq!(single_line, Comment::from(" hello world\n    long indent"));

        let string = LocatedSpan::new("/* foo\n   bar   \n   baz\n*/");
        let (_, multi_line) = all_consuming(comment)(string).expect("multi-line comment failed");
        assert_eq!(multi_line, Comment::from(" foo\n   bar   \n   baz"));
    }

    #[test]
    fn spaces() {
        let string = LocatedSpan::new(" \n\r\t# line comment\n/* block comment */stop here");
        let (rest, _) = space(string).expect("all spaces failed");
        assert_eq!(
            rest,
            LocatedSpan {
                offset: 38,
                line: 3,
                fragment: "stop here",
                extra: (),
            }
        );

        let string = LocatedSpan::new("    # foo\n    /*\n  bar\n*/\n    # stop here\nbaz");
        let (rest, _) = space_until_final_comment(string).expect("spaces till last comment failed");
        assert_eq!(
            rest,
            LocatedSpan {
                offset: 30,
                line: 5,
                fragment: "# stop here\nbaz",
                extra: (),
            }
        );

        let string = LocatedSpan::new(" \n\r\tbaz");
        let (rest, _) = space_until_final_comment(string).expect("spaces without comments failed");
        assert_eq!(
            rest,
            LocatedSpan {
                offset: 4,
                line: 2,
                fragment: "baz",
                extra: (),
            }
        );
    }

    #[test]
    fn identifiers() {
        let string = LocatedSpan::new("fooBAR123-_'");
        let (_, single) = all_consuming(identifier)(string).expect("single identifier failed");
        assert_eq!(single, Ident::from("fooBAR123-_'"));

        let string = LocatedSpan::new("let");
        all_consuming(identifier)(string).expect_err("should reject keywords");

        let string = LocatedSpan::new("foo.bar.baz");
        let (_, path) = all_consuming(ident_path)(string).expect("identifier path failed");
        assert_eq!(path, nix_token!(foo.bar.baz));
    }
}
