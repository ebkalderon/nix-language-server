pub use self::keywords::*;
pub use self::literal::literal;

use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not};
use nom::character::complete::{alpha1, alphanumeric1, char, multispace1, not_line_ending};
use nom::combinator::{map, map_parser, recognize, verify};
use nom::multi::{count, many0, separated_nonempty_list};
use nom::sequence::{pair, preceded};

use super::{map_spanned, IResult, Span};
use crate::ast::tokens::{Ident, IdentPath};

mod keywords;
mod literal;

pub fn space(input: Span) -> IResult<()> {
    let comment = preceded(char('#'), not_line_ending);
    let whitespace = multispace1;
    map(many0(alt((comment, whitespace))), |_| ())(input)
}

pub fn identifier(input: Span) -> IResult<Ident> {
    let first = count(alt((alpha1, is_a("_-"))), 1);
    let rest = many0(alt((alphanumeric1, is_a("_-'"))));
    let ident = verify(recognize(pair(first, rest)), |s| !is_keyword(s));
    map_spanned(input, ident, |(s, span)| Ident::from((s.fragment, span)))(input)
}

pub fn ident_path(input: Span) -> IResult<IdentPath> {
    let ident_path = separated_nonempty_list(char('.'), map_parser(is_not("."), identifier));
    map_spanned(input, ident_path, IdentPath::from)(input)
}
