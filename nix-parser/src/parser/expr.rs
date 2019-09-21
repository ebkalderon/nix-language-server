use codespan::Span;
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use std::iter::{self, FromIterator};

use super::partial::{expect_terminated, map_partial, map_partial_spanned, pair_partial, Partial};
use super::{map_spanned, tokens, IResult, LocatedSpan};
use crate::ast::{BinaryOp, Expr, ExprBinary, ExprFnApp, ExprIf, ExprUnary, UnaryOp};
use crate::HasSpan;

mod atomic;
mod bind;
mod stmt;

pub fn expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    terminated(stmt, tokens::space)(input)
}

fn stmt(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let with = map_partial(stmt::with, Expr::With);
    let assert = map_partial(stmt::assert, Expr::Assert);
    let let_in = map_partial(stmt::let_in, Expr::LetIn);
    alt((with, assert, let_in, if_else))(input)
}

fn if_else(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let keyword_if = pair(tokens::keyword_if, tokens::space1);
    let keyword_then = pair(tokens::keyword_then, tokens::space1);
    let keyword_else = pair(tokens::keyword_else, tokens::space1);

    let cond = expect_terminated(map_partial(expr, Box::new), keyword_then);
    let body = expect_terminated(map_partial(expr, Box::new), keyword_else);
    let fallback = map_partial(expr, Box::new);

    let expr = preceded(keyword_if, pair_partial(cond, pair_partial(body, fallback)));
    let if_else = map_partial_spanned(expr, |span, (cond, (body, fallback))| {
        Expr::If(ExprIf::new(cond, body, fallback, span))
    });

    alt((if_else, imply))(input)
}

fn imply(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let imply = tuple((tokens::space1, tokens::op_imply, tokens::space));
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
    let and = tuple((tokens::space, tokens::op_and, tokens::space));
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
    let or = tuple((tokens::space, tokens::op_or, tokens::space));
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
    let eq = map(tokens::op_eq, |_| BinaryOp::Eq);
    let neq = map(tokens::op_neq, |_| BinaryOp::NotEq);

    let op = delimited(tokens::space, alt((eq, neq)), tokens::space);
    let expr = pair(compare, opt(pair(op, compare)));
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

fn compare(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let lte = map(tokens::op_lte, |_| BinaryOp::LessThanEq);
    let lt = map(tokens::op_lt, |_| BinaryOp::LessThan);
    let gte = map(tokens::op_gte, |_| BinaryOp::LessThanEq);
    let gt = map(tokens::op_gt, |_| BinaryOp::LessThan);

    let op = delimited(tokens::space, alt((lte, lt, gte, gt)), tokens::space);
    let expr = pair(update, opt(pair(op, update)));
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

fn update(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let op = delimited(tokens::space, tokens::op_update, tokens::space);
    let expr = pair(sum, many0(preceded(op, sum)));
    map(expr, |(first, rest)| {
        let exprs = Partial::from_iter(iter::once(first).chain(rest));
        exprs.map(|mut exprs| {
            let last = exprs.pop().unwrap();
            exprs.into_iter().rev().fold(last, |lhs, rhs| {
                let span = Span::merge(rhs.span(), lhs.span());
                let expr = ExprBinary::new(BinaryOp::Update, Box::new(rhs), Box::new(lhs), span);
                Expr::Binary(expr)
            })
        })
    })(input)
}

fn sum(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let add = map(tokens::op_add, |_| BinaryOp::Add);
    let sub = map(tokens::op_sub, |_| BinaryOp::Sub);
    let op = delimited(tokens::space, alt((add, sub)), tokens::space);
    let expr = pair(product, many0(pair(op, product)));
    map(expr, |(first, rest)| {
        rest.into_iter().fold(first, |lhs, (op, rhs)| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprBinary::new(op, Box::new(lhs), Box::new(rhs), span);
                    Expr::Binary(expr)
                })
            })
        })
    })(input)
}

fn product(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let mul = map(tokens::op_mul, |_| BinaryOp::Mul);
    let div = map(tokens::op_div, |_| BinaryOp::Div);
    let op = delimited(tokens::space, alt((mul, div)), tokens::space);
    let expr = pair(concat, many0(pair(op, concat)));
    map(expr, |(first, rest)| {
        rest.into_iter().fold(first, |lhs, (op, rhs)| {
            lhs.flat_map(|lhs| {
                rhs.map(|rhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    let expr = ExprBinary::new(op, Box::new(lhs), Box::new(rhs), span);
                    Expr::Binary(expr)
                })
            })
        })
    })(input)
}

fn concat(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let op = delimited(tokens::space, tokens::op_concat, tokens::space);
    let expr = pair(unary, many0(preceded(op, unary)));
    map(expr, |(first, rest)| {
        let exprs = Partial::from_iter(iter::once(first).chain(rest));
        exprs.map(|mut exprs| {
            let last = exprs.pop().unwrap();
            exprs.into_iter().rev().fold(last, |lhs, rhs| {
                let span = Span::merge(rhs.span(), lhs.span());
                let expr = ExprBinary::new(BinaryOp::Concat, Box::new(rhs), Box::new(lhs), span);
                Expr::Binary(expr)
            })
        })
    })(input)
}

fn unary(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let neg = map(tokens::op_sub, |_| UnaryOp::Neg);
    let not = map(tokens::op_not, |_| UnaryOp::Not);
    let expr = pair(opt(alt((neg, not))), fn_app);
    map_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => expr.map(|expr| Expr::Unary(ExprUnary::new(op, Box::new(expr), span))),
        None => expr,
    })(input)
}

fn fn_app(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let expr = pair(atomic, many0(preceded(tokens::space1, atomic)));
    map(expr, |(first, rest)| {
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

fn atomic(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map(map(tokens::literal, Expr::Literal), Partial::from);
    let rec_set = map_partial(atomic::let_set, Expr::Let);
    let let_set = map_partial(atomic::rec_set, Expr::Rec);
    let attr = map(map(tokens::ident_path, Expr::Attr), Partial::from);
    alt((paren, set, list, literal, rec_set, let_set, attr))(input)
}
