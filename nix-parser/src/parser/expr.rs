use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{pair, preceded};

use super::partial::{map_partial, map_partial_spanned, pair_partial, Partial};
use super::{tokens, IResult};
use crate::ast::{Expr, ExprFnApp, ExprUnary, UnaryOp};
use crate::error::{Errors, UnexpectedError};
use crate::lexer::{Token, Tokens};
use crate::{HasSpan, ToSpan};

mod atomic;
mod bind;

pub fn expr(input: Tokens) -> IResult<Partial<Expr>> {
    preceded(many0(tokens::comment), unary)(input)
}

fn unary(input: Tokens) -> IResult<Partial<Expr>> {
    let neg = map(tokens::op_sub, |_| UnaryOp::Neg);
    let not = map(tokens::op_not, |_| UnaryOp::Not);
    let unary = map(opt(alt((neg, not))), Partial::from);
    let expr = pair_partial(unary, fn_app);
    map_partial_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => Expr::Unary(ExprUnary::new(op, Box::new(expr), span)),
        None => expr,
    })(input)
}

fn fn_app(input: Tokens) -> IResult<Partial<Expr>> {
    map(pair(atomic, many0(atomic)), |(first, rest)| {
        rest.into_iter().fold(first, |lhs, rhs| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprFnApp::new(Box::new(lhs), Box::new(rhs), span);
                    Expr::FnApp(expr)
                })
            })
        })
    })(input)
}

fn atomic(input: Tokens) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map_partial(atomic::literal, Expr::Literal);
    alt((paren, set, list, literal))(input)
}

fn unknown(input: Tokens) -> IResult<Partial<Expr>> {
    let (remaining, tokens) = take(1usize)(input)?;
    let mut errors = Errors::new();
    errors.push(UnexpectedError::new(
        tokens.current().description(),
        tokens.to_span(),
    ));
    if let Token::Eof(_) = tokens.current() {
        Err(nom::Err::Error(errors))
    } else {
        let error = Expr::Error(tokens.to_span());
        Ok((remaining, Partial::with_errors(Some(error), errors)))
    }
}
