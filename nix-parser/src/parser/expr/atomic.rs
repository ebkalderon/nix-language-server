use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::preceded;

use super::{expr, unary};
use crate::ast::tokens::Literal;
use crate::ast::{ExprList, ExprParen};
use crate::lexer::Tokens;
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult};

pub fn paren(input: Tokens) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(tokens::paren_left, expr), tokens::paren_right);
    map_partial_spanned(paren, |span, inner| ExprParen::new(Box::new(inner), span))(input)
}

pub fn list(input: Tokens) -> IResult<Partial<ExprList>> {
    let elems = many_till_partial(unary, tokens::bracket_right);
    let list = expect_terminated(preceded(tokens::bracket_left, elems), tokens::bracket_right);
    map_partial_spanned(list, |span, exprs| ExprList::new(exprs, span))(input)
}

pub fn literal(input: Tokens) -> IResult<Partial<Literal>> {
    let boolean = map(tokens::boolean, Partial::from);
    let null = map(tokens::null, Partial::from);
    let path = map(tokens::path, Partial::from);
    let float = map(tokens::float, Partial::from);
    let integer = map(tokens::integer, Partial::from);
    let path_template = map(tokens::path_template, Partial::from);
    let uri = map(tokens::uri, Partial::from);
    alt((boolean, null, float, integer, path, path_template, uri))(input)
}
