use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, space1};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use super::partial::{map_partial, Partial};
use super::{map_spanned, tokens, IResult, LocatedSpan};
use crate::ast::{BinaryOp, Expr, ExprBinary, ExprFnApp, ExprUnary, UnaryOp};
use crate::HasSpan;

mod atomic;
mod bind;

pub fn expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    terminated(imply, tokens::space)(input)
}

fn imply(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let imply = tuple((tokens::space1, tag("->"), tokens::space));
    let expr = pair(and, many0(preceded(imply, and)));
    map(expr, |(first, rest)| {
        rest.into_iter().fold(first, |lhs, rhs| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprBinary::new(BinaryOp::Impl, Box::new(lhs), Box::new(rhs), span);
                    Expr::Binary(expr)
                })
            })
        })
    })(input)
}

fn and(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let and = tuple((tokens::space, tag("&&"), tokens::space));
    let expr = pair(or, many0(preceded(and, or)));
    map(expr, |(first, rest)| {
        rest.into_iter().fold(first, |lhs, rhs| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprBinary::new(BinaryOp::And, Box::new(lhs), Box::new(rhs), span);
                    Expr::Binary(expr)
                })
            })
        })
    })(input)
}

fn or(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let or = tuple((tokens::space, tag("||"), tokens::space));
    let expr = pair(equality, many0(preceded(or, equality)));
    map(expr, |(first, rest)| {
        rest.into_iter().fold(first, |lhs, rhs| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprBinary::new(BinaryOp::Or, Box::new(lhs), Box::new(rhs), span);
                    Expr::Binary(expr)
                })
            })
        })
    })(input)
}

fn equality(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let eq = map(tag("=="), |_| BinaryOp::Eq);
    let neq = map(tag("!="), |_| BinaryOp::NotEq);

    let op = delimited(tokens::space, alt((eq, neq)), tokens::space);
    let expr = pair(unary, opt(pair(op, unary)));
    map(expr, |(lhs, op)| match op {
        None => lhs,
        Some((op, rhs)) => lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                let expr = ExprBinary::new(op, Box::new(lhs), Box::new(rhs), span);
                Expr::Binary(expr)
            })
        }),
    })(input)
}

fn unary(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let neg = map(char('-'), |_| UnaryOp::Neg);
    let not = map(char('!'), |_| UnaryOp::Not);
    let expr = pair(opt(alt((neg, not))), atomic);
    map_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => expr.map(|expr| Expr::Unary(ExprUnary::new(op, Box::new(expr), span))),
        None => expr,
    })(input)
}

fn atomic(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map(map(tokens::literal, Expr::Literal), Partial::from);
    let attr = map(map(tokens::ident_path, Expr::Attr), Partial::from);
    alt((paren, set, list, literal, attr))(input)
}
