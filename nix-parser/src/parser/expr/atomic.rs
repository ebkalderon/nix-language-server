use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::preceded;

use super::{bind, expr, unary};
use crate::ast::tokens::Literal;
use crate::ast::{Bind, ExprLet, ExprList, ExprParen, ExprRec, ExprSet};
use crate::lexer::Tokens;
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult};

pub fn paren(input: Tokens) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(tokens::paren_left, expr), tokens::paren_right);
    map_partial_spanned(paren, |span, inner| ExprParen::new(Box::new(inner), span))(input)
}

pub fn set(input: Tokens) -> IResult<Partial<ExprSet>> {
    map_partial_spanned(set_binds, |span, binds| ExprSet::new(binds, span))(input)
}

pub fn rec_set(input: Tokens) -> IResult<Partial<ExprRec>> {
    let rec_set = preceded(tokens::keyword_rec, set_binds);
    map_partial_spanned(rec_set, |span, binds| ExprRec::new(binds, span))(input)
}

pub fn let_set(input: Tokens) -> IResult<Partial<ExprLet>> {
    let let_set = preceded(tokens::keyword_let, set_binds);
    map_partial_spanned(let_set, |span, binds| ExprLet::new(binds, span))(input)
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

fn set_binds(input: Tokens) -> IResult<Partial<Vec<Bind>>> {
    let binds = many_till_partial(bind::bind, alt((tokens::brace_right, tokens::semi)));
    expect_terminated(preceded(tokens::brace_left, binds), tokens::brace_right)(input)
}
