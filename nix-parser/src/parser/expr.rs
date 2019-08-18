use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::terminated;

use super::partial::{map_partial, Partial};
use super::{tokens, IResult, Span};
use crate::ast::Expr;

mod atomic;
mod bind;

pub fn expr(input: Span) -> IResult<Partial<Expr>> {
    terminated(atomic, tokens::space)(input)
}

fn atomic(input: Span) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let literal = map(map(tokens::literal, Expr::Literal), Partial::from);
    let attr = map(map(tokens::ident_path, Expr::Attr), Partial::from);
    alt((paren, set, literal, attr))(input)
}
