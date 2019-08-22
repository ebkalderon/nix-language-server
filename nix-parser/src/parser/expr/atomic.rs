use std::iter::FromIterator;

use nom::branch::alt;
use nom::character::complete::{anychar, char};
use nom::combinator::{cut, map, opt, peek};
use nom::error::ErrorKind;
use nom::error::VerboseError;
use nom::multi::{many0, many_till};
use nom::sequence::{pair, preceded, terminated};

use super::{bind, expr, unary};
use crate::ast::{ExprList, ExprParen, ExprSet};
use crate::parser::partial::{expect_terminated, map_err_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult, Span};

pub fn paren(input: Span) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(pair(char('('), tokens::space), expr), char(')'));
    map_partial_spanned(paren, |span, e| ExprParen::new(Box::new(e), span))(input)
}

pub fn set(input: Span) -> IResult<Partial<ExprSet>> {
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let binds = preceded(char('{'), many_till(bind, pair(tokens::space, char('}'))));
    let set = map(binds, |(p, _)| Partial::from_iter(p));
    map_partial_spanned(set, |span, binds| ExprSet::new(binds, span))(input)
}

pub fn list(input: Span) -> IResult<Partial<ExprList>> {
    let elem = terminated(unary, tokens::space);
    let elems = map(many_till(elem, char(']')), |(p, _)| Partial::from_iter(p));
    let list = preceded(pair(char('['), tokens::space), elems);
    map_partial_spanned(list, |span, exprs| ExprList::new(exprs, span))(input)
}
