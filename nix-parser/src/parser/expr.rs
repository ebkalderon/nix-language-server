use std::iter::{self, FromIterator};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{pair, preceded};

use super::partial::{expect_terminated, map_partial, map_partial_spanned, pair_partial, Partial};
use super::{tokens, IResult};
use crate::ast::{BinaryOp, Expr, ExprBinary, ExprFnApp, ExprIf, ExprUnary, UnaryOp};
use crate::error::{Errors, UnexpectedError};
use crate::lexer::{Token, Tokens};
use crate::{HasSpan, ToSpan};

mod atomic;
mod bind;
mod stmt;
mod util;

pub fn expr(input: Tokens) -> IResult<Partial<Expr>> {
    preceded(many0(tokens::comment), stmt)(input)
}

fn stmt(input: Tokens) -> IResult<Partial<Expr>> {
    let with = map_partial(stmt::with, Expr::With);
    let assert = map_partial(stmt::assert, Expr::Assert);
    let let_in = map_partial(stmt::let_in, Expr::LetIn);
    alt((with, assert, let_in, if_else))(input)
}

fn if_else(input: Tokens) -> IResult<Partial<Expr>> {
    let found = "keyword `then`";
    let cond = alt((expr, util::error_expr_if(tokens::keyword_then, found)));
    let cond_then = expect_terminated(map_partial(cond, Box::new), tokens::keyword_then);
    let if_cond_then = preceded(tokens::keyword_if, cond_then);

    let found = "keyword `else`";
    let body = alt((expr, util::error_expr_if(tokens::keyword_else, found)));
    let body_else = expect_terminated(map_partial(body, Box::new), tokens::keyword_else);

    let expr = alt((expr, util::error_expr_if(tokens::eof, "<eof>")));
    let fallback = map_partial(expr, Box::new);
    let block = pair_partial(if_cond_then, pair_partial(body_else, fallback));
    let if_else = map_partial_spanned(block, |span, (cond, (body, fallback))| {
        Expr::If(ExprIf::new(cond, body, fallback, span))
    });

    alt((if_else, imply))(input)
}

fn imply(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(and, many0(preceded(tokens::op_imply, and)));
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

fn and(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(or, many0(preceded(tokens::op_and, or)));
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

fn or(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(equality, many0(preceded(tokens::op_or, equality)));
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

fn equality(input: Tokens) -> IResult<Partial<Expr>> {
    let eq = map(tokens::op_eq, |_| BinaryOp::Eq);
    let neq = map(tokens::op_neq, |_| BinaryOp::NotEq);
    let expr = pair(compare, opt(pair(alt((eq, neq)), compare)));
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

fn compare(input: Tokens) -> IResult<Partial<Expr>> {
    let lte = map(tokens::op_lte, |_| BinaryOp::LessThanEq);
    let lt = map(tokens::op_lt, |_| BinaryOp::LessThan);
    let gte = map(tokens::op_gte, |_| BinaryOp::GreaterThanEq);
    let gt = map(tokens::op_gt, |_| BinaryOp::GreaterThan);
    let expr = pair(update, opt(pair(alt((lte, lt, gte, gt)), update)));
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

fn update(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(sum, many0(preceded(tokens::op_update, sum)));
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

fn sum(input: Tokens) -> IResult<Partial<Expr>> {
    let add = map(tokens::op_add, |_| BinaryOp::Add);
    let sub = map(tokens::op_sub, |_| BinaryOp::Sub);
    let expr = pair(product, many0(pair(alt((add, sub)), product)));
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

fn product(input: Tokens) -> IResult<Partial<Expr>> {
    let mul = map(tokens::op_mul, |_| BinaryOp::Mul);
    let div = map(tokens::op_div, |_| BinaryOp::Div);
    let expr = pair(concat, many0(pair(alt((mul, div)), concat)));
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

fn concat(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(unary, many0(preceded(tokens::op_concat, unary)));
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

fn unary(input: Tokens) -> IResult<Partial<Expr>> {
    let neg = map(tokens::op_sub, |_| UnaryOp::Neg);
    let not = map(tokens::op_not, |_| UnaryOp::Not);
    let unary = pair_partial(map(opt(alt((neg, not))), Partial::from), fn_app);
    let expr = map_partial_spanned(unary, |span, (unary, expr)| match unary {
        Some(op) => Expr::Unary(ExprUnary::new(op, Box::new(expr), span)),
        None => expr,
    });
    alt((expr, error))(input)
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
    let rec_set = map_partial(atomic::rec_set, Expr::Rec);
    let let_set = map_partial(atomic::let_set, Expr::Let);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map_partial(atomic::literal, Expr::Literal);
    alt((paren, set, rec_set, let_set, list, literal))(input)
}

fn error(input: Tokens) -> IResult<Partial<Expr>> {
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
