pub use self::keywords::*;
pub use self::literal::literal;

use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_until, take_while};
use nom::character::complete::{
    anychar, char, line_ending, multispace0, multispace1, not_line_ending, space0,
};
use nom::character::{is_alphabetic, is_alphanumeric};
use nom::combinator::{cut, map, recognize, verify};
use nom::multi::{many0, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Slice;

use super::{IResult, Span};
use crate::ast::tokens::{Comment, Ident, IdentPath};

mod keywords;
mod literal;

pub fn comment(input: Span) -> IResult<Comment> {
    let span = map(not_line_ending, |s: Span| s.fragment);
    let rows = separated_nonempty_list(pair(line_ending, space0), preceded(char('#'), span));
    let line = map(rows, |r| r.join("\n"));

    let span = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    let rows = map(span, |s: Span| s.fragment.lines().collect::<Vec<_>>());
    let block = map(rows, |r| r.join("\n"));

    let (remainder, comment) = alt((line, block))(input)?;
    let comment_len = remainder.offset - input.offset;
    let span = input.slice(..comment_len);

    Ok((remainder, Comment::from((comment, span))))
}

pub fn space(input: Span) -> IResult<()> {
    let line = delimited(char('#'), not_line_ending, line_ending);
    let block = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    map(many0(alt((multispace1, line, block))), |_| ())(input)
}

pub fn space_until_final_comment(input: Span) -> IResult<()> {
    let trimmed_comment = terminated(recognize(comment), multispace0);
    let (remainder, comments) = preceded(multispace0, many0(trimmed_comment))(input)?;

    let chars_to_take = comments
        .last()
        .map(|comment| comment.offset - input.offset)
        .unwrap_or_else(|| remainder.offset - input.offset);

    map(take(chars_to_take), |_| ())(input)
}

pub fn identifier(input: Span) -> IResult<Ident> {
    let first = verify(anychar, |c| is_alphabetic(*c as u8) || *c == '_');
    let rest = take_while(|c: char| is_alphanumeric(c as u8) || "_-'".contains(c));
    let ident = recognize(pair(first, rest));
    let verified = verify(ident, |span| !is_keyword(span));
    map(verified, |span: Span| Ident::from((span.fragment, span)))(input)
}

pub fn ident_path(input: Span) -> IResult<IdentPath> {
    let ident_path = separated_nonempty_list(char('.'), identifier);
    map(ident_path, |idents| IdentPath::from((idents, input)))(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn comments() {
        let string = Span::new("# hello world\n    #    long indent");
        let (_, single_line) = all_consuming(comment)(string).expect("single-line comment failed");
        assert_eq!(single_line, Comment::from(" hello world\n    long indent"));

        let string = Span::new("/* foo\n   bar   \n   baz\n*/");
        let (_, multi_line) = all_consuming(comment)(string).expect("multi-line comment failed");
        assert_eq!(multi_line, Comment::from(" foo\n   bar   \n   baz"));
    }

    #[test]
    fn comsuming_spaces() {
        let string = Span::new(" \n\r\t# line comment\n/* block comment */stop here");
        let (rest, _) = space(string).expect("all spaces failed");
        assert_eq!(
            rest,
            Span {
                offset: 38,
                line: 3,
                fragment: "stop here",
                extra: (),
            }
        );

        let string = Span::new("    # foo\n    /*\n  bar\n*/\n    # stop here\nbaz");
        let (rest, _) = space_until_final_comment(string).expect("spaces till last comment failed");
        assert_eq!(
            rest,
            Span {
                offset: 30,
                line: 5,
                fragment: "# stop here\nbaz",
                extra: (),
            }
        );

        let string = Span::new(" \n\r\tbaz");
        let (rest, _) = space_until_final_comment(string).expect("spaces without comments failed");
        assert_eq!(
            rest,
            Span {
                offset: 4,
                line: 2,
                fragment: "baz",
                extra: (),
            }
        );
    }
}
