pub use self::keywords::*;
pub use self::literal::literal;

use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_until, take_while};
use nom::character::complete::{alpha1, char, multispace0, multispace1, not_line_ending, space0};
use nom::character::is_alphanumeric;
use nom::combinator::{cut, map, recognize, verify};
use nom::error::context;
use nom::multi::{count, many0, separated_nonempty_list};
use nom::sequence::{delimited, pair, preceded};

use super::{IResult, Span};
use crate::ast::tokens::{Comment, Ident, IdentPath};

mod keywords;
mod literal;

pub fn comment(input: Span) -> IResult<Comment> {
    let span = map(not_line_ending, |s: Span| s.fragment);
    let rows = separated_nonempty_list(multispace0, preceded(pair(char('#'), space0), span));
    let text = map(rows, |r| r.join("\n"));
    let single = map(text, |text| Comment::from((text, input)));

    let span = delimited(tag("/*"), recognize(take_until("*/")), cut(tag("*/")));
    let rows = map(span, |s: Span| s.fragment.lines().map(|l| l.trim_start()));
    let text = map(rows, |r| r.collect::<Vec<_>>().join("\n"));
    let multiple = map(text, |text| Comment::from((text, input)));

    alt((single, multiple))(input)
}

pub fn space(input: Span) -> IResult<()> {
    let comment = recognize(comment);
    map(many0(alt((comment, multispace1))), |_| ())(input)
}

pub fn identifier(input: Span) -> IResult<Ident> {
    let first = cut(count(alt((alpha1, is_a("_"))), 1));
    let rest = take_while(is_identifier_char);
    let ident = recognize(pair(first, rest));
    let verified = verify(ident, |span| !is_keyword(span));
    map(verified, |span: Span| Ident::from((span.fragment, span)))(input)
}

pub fn ident_path(input: Span) -> IResult<IdentPath> {
    let ident_path = separated_nonempty_list(char('.'), identifier);
    map(ident_path, |idents| IdentPath::from((idents, input)))(input)
}

fn is_identifier_char(c: char) -> bool {
    is_alphanumeric(c as u8) || "_-'".contains(c)
}
