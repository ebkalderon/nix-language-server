use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::terminated;

use self::atomic::paren;
use super::{tokens, IResult, Span};
use crate::ast::Expr;

mod atomic;
pub(crate) mod bind;

pub fn expr(input: Span) -> IResult<Expr> {
    terminated(atomic, tokens::space)(input)
}

fn atomic(input: Span) -> IResult<Expr> {
    let paren = map(paren, Expr::Paren);
    let literal = map(tokens::literal, Expr::Literal);
    let attr = map(tokens::ident_path, Expr::Attr);
    alt((paren, literal, attr))(input)
}
