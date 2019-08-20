use std::iter::FromIterator;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::char;
use nom::combinator::{cut, map, map_parser, recognize};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::{many0, many_till};
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{bind, expr};
use crate::ast::{ExprParen, ExprSet};
use crate::parser::partial::{expect_terminated, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn paren(input: Span) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(pair(char('('), tokens::space), expr), char(')'));
    map_partial_spanned(input, paren, |span, e| ExprParen::new(Box::new(e), span))(input)
}

pub fn set(input: Span) -> IResult<Partial<ExprSet>> {
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let binds = preceded(char('{'), many_till(bind, pair(tokens::space, char('}'))));;
    let set = map(binds, |(p, _)| Partial::from_iter(p));
    map_partial_spanned(input, set, |span, binds| ExprSet::new(binds, span))(input)
}
