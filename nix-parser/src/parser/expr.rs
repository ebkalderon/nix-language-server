use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::sequence::{pair, terminated};

use super::partial::{map_partial, Partial};
use super::{map_spanned, tokens, IResult, Span};
use crate::ast::{Expr, ExprUnary, UnaryOp};

mod atomic;
mod bind;

pub fn expr(input: Span) -> IResult<Partial<Expr>> {
    terminated(unary, tokens::space)(input)
}

fn unary(input: Span) -> IResult<Partial<Expr>> {
    let neg = map(char('-'), |_| UnaryOp::Neg);
    let not = map(char('!'), |_| UnaryOp::Not);
    let expr = pair(opt(alt((neg, not))), atomic);
    map_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => expr.map(|expr| Expr::Unary(ExprUnary::new(op, Box::new(expr), span))),
        None => expr,
    })(input)
}

fn atomic(input: Span) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map(map(tokens::literal, Expr::Literal), Partial::from);
    let attr = map(map(tokens::ident_path, Expr::Attr), Partial::from);
    alt((paren, set, list, literal, attr))(input)
}
