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

use super::{IResult, Span};
use crate::ast::tokens::{Comment, Ident, IdentPath};

mod keywords;
mod literal;

pub fn comment(input: Span) -> IResult<Comment> {
    let span = map(not_line_ending, |s: Span| s.fragment);
    let rows = separated_nonempty_list(pair(line_ending, space0), preceded(char('#'), span));
    let text = map(rows, |r| r.join("\n"));
    let single = map(text, |text| Comment::from((text, input)));

    let span = delimited(tag("/*"), take_until("*/"), cut(tag("*/")));
    let rows = map(span, |s: Span| s.fragment.trim().lines().map(|l| l.trim()));
    let text = map(rows, |r| r.collect::<Vec<_>>().join("\n"));
    let multiple = map(text, |text| Comment::from((text, input)));

    alt((single, multiple))(input)
}

pub fn space(input: Span) -> IResult<()> {
    let comment = recognize(comment);
    map(many0(alt((comment, multispace1))), |_| ())(input)
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
