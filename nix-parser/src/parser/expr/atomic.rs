use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::preceded;

use super::expr;
use crate::ast::{Expr, ExprParen};
use crate::lexer::Tokens;
use crate::parser::partial::{expect_terminated, map_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult};

pub fn paren(input: Tokens) -> IResult<Partial<Expr>> {
    let paren = expect_terminated(preceded(tokens::paren_left, expr), tokens::paren_right);
    let expr = map_partial_spanned(paren, |span, inner| ExprParen::new(Box::new(inner), span));
    map_partial(expr, Expr::Paren)(input)
}

pub fn literal(input: Tokens) -> IResult<Partial<Expr>> {
    let boolean = map(map(tokens::boolean, Expr::Literal), Partial::from);
    let null = map(map(tokens::null, Expr::Literal), Partial::from);
    let path = map(map(tokens::path, Expr::Literal), Partial::from);
    let float = map(map(tokens::float, Expr::Literal), Partial::from);
    let integer = map(map(tokens::integer, Expr::Literal), Partial::from);
    let path_template = map(map(tokens::path_template, Expr::Literal), Partial::from);
    let uri = map(map(tokens::uri, Expr::Literal), Partial::from);
    alt((boolean, null, float, integer, path, path_template, uri))(input)
}
