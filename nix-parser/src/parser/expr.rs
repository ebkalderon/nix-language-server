use std::iter::{self, FromIterator};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{pair, preceded};

use super::partial::{
    expect_terminated, map_partial, map_partial_spanned, opt_partial, pair_partial, Partial,
};
use super::{tokens, IResult};
use crate::ast::tokens::Ident;
use crate::ast::{BinaryOp, Expr, ExprBinary, ExprFnApp, ExprIf, ExprProj, ExprUnary, UnaryOp};
use crate::error::{Errors, UnexpectedError};
use crate::lexer::{Token, Tokens};
use crate::{HasSpan, ToSpan};

mod atomic;
mod attr;
mod bind;
mod func;
mod stmt;
mod util;

pub fn expr(input: Tokens) -> IResult<Partial<Expr>> {
    preceded(many0(tokens::comment), function)(input)
}

fn function(input: Tokens) -> IResult<Partial<Expr>> {
    let function = map_partial(func::fn_decl, Expr::from);
    let with = map_partial(stmt::with, Expr::from);
    let assert = map_partial(stmt::assert, Expr::from);
    let let_in = map_partial(stmt::let_in, Expr::from);
    alt((function, with, assert, let_in, if_else))(input)
}

fn if_else(input: Tokens) -> IResult<Partial<Expr>> {
    let cond = alt((util::error_expr_if(tokens::keyword_then), expr));
    let cond_then = expect_terminated(cond, tokens::keyword_then);
    let if_cond_then = preceded(tokens::keyword_if, cond_then);

    let body = alt((util::error_expr_if(tokens::keyword_else), expr));
    let body_else = expect_terminated(body, tokens::keyword_else);

    let expr = alt((util::error_expr_if(tokens::eof), expr));
    let block = pair_partial(if_cond_then, pair_partial(body_else, expr));
    let if_else = map_partial_spanned(block, |span, (cond, (body, fallback))| {
        Expr::from(ExprIf::new(cond, body, fallback, span))
    });

    alt((if_else, imply))(input)
}

fn imply(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = and(input)?;
    let next = alt((and, util::error_expr_if(tokens::eof)));
    util::fold_many0(preceded(tokens::op_imply, next), first, |lhs, rhs| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(BinaryOp::Impl, lhs, rhs, span))
            })
        })
    })(input)
}

fn and(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = or(input)?;
    let next = alt((or, util::error_expr_if(tokens::eof)));
    util::fold_many0(preceded(tokens::op_and, next), first, |lhs, rhs| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(BinaryOp::And, lhs, rhs, span))
            })
        })
    })(input)
}

fn or(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = equality(input)?;
    let next = alt((equality, util::error_expr_if(tokens::eof)));
    util::fold_many0(preceded(tokens::op_or, next), first, |lhs, rhs| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(BinaryOp::Or, lhs, rhs, span))
            })
        })
    })(input)
}

fn equality(input: Tokens) -> IResult<Partial<Expr>> {
    let eq = map(tokens::op_eq, |_| BinaryOp::Eq);
    let neq = map(tokens::op_neq, |_| BinaryOp::NotEq);
    let rhs = alt((compare, util::error_expr_if(tokens::eof)));
    let expr = pair(compare, opt(pair(alt((eq, neq)), rhs)));
    map(expr, |(lhs, op)| match op {
        None => lhs,
        Some((op, rhs)) => lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(op, lhs, rhs, span))
            })
        }),
    })(input)
}

fn compare(input: Tokens) -> IResult<Partial<Expr>> {
    let lte = map(tokens::op_lte, |_| BinaryOp::LessThanEq);
    let lt = map(tokens::op_lt, |_| BinaryOp::LessThan);
    let gte = map(tokens::op_gte, |_| BinaryOp::GreaterThanEq);
    let gt = map(tokens::op_gt, |_| BinaryOp::GreaterThan);

    let rhs = alt((update, util::error_expr_if(tokens::eof)));
    let expr = pair(update, opt(pair(alt((lte, lt, gte, gt)), rhs)));
    map(expr, |(lhs, op)| match op {
        None => lhs,
        Some((op, rhs)) => lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(op, lhs, rhs, span))
            })
        }),
    })(input)
}

fn update(input: Tokens) -> IResult<Partial<Expr>> {
    let rhs = alt((sum, util::error_expr_if(tokens::eof)));
    let expr = pair(sum, many0(preceded(tokens::op_update, rhs)));
    map(expr, |(first, rest)| {
        if rest.is_empty() {
            first
        } else {
            let exprs = Partial::from_iter(iter::once(first).chain(rest));
            exprs.map(|mut exprs| {
                let last = exprs.pop().unwrap();
                exprs.into_iter().rev().fold(last, |rhs, lhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    Expr::from(ExprBinary::new(BinaryOp::Update, lhs, rhs, span))
                })
            })
        }
    })(input)
}

fn sum(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = product(input)?;
    let next = alt((product, util::error_expr_if(tokens::eof)));
    let add = map(tokens::op_add, |_| BinaryOp::Add);
    let sub = map(tokens::op_sub, |_| BinaryOp::Sub);
    util::fold_many0(pair(alt((add, sub)), next), first, |lhs, (op, rhs)| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(op, lhs, rhs, span))
            })
        })
    })(input)
}

fn product(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = concat(input)?;
    let next = alt((concat, util::error_expr_if(tokens::eof)));
    let mul = map(tokens::op_mul, |_| BinaryOp::Mul);
    let div = map(tokens::op_div, |_| BinaryOp::Div);
    util::fold_many0(pair(alt((mul, div)), next), first, |lhs, (op, rhs)| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(op, lhs, rhs, span))
            })
        })
    })(input)
}

fn concat(input: Tokens) -> IResult<Partial<Expr>> {
    let expr = pair(has_attr, many0(preceded(tokens::op_concat, has_attr)));
    map(expr, |(first, rest)| {
        if rest.is_empty() {
            first
        } else {
            let exprs = Partial::from_iter(iter::once(first).chain(rest));
            exprs.map(|mut exprs| {
                let last = exprs.pop().unwrap();
                exprs.into_iter().rev().fold(last, |rhs, lhs| {
                    let span = Span::merge(lhs.span(), rhs.span());
                    Expr::from(ExprBinary::new(BinaryOp::Concat, lhs, rhs, span))
                })
            })
        }
    })(input)
}

fn has_attr(input: Tokens) -> IResult<Partial<Expr>> {
    let rhs = alt((unary, util::error_expr_if(tokens::eof)));
    let expr = pair(unary, opt(preceded(tokens::op_question, rhs)));
    map(expr, |(lhs, rhs)| match rhs {
        None => lhs,
        Some(rhs) => lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprBinary::new(BinaryOp::HasAttr, lhs, rhs, span))
            })
        }),
    })(input)
}

fn unary(input: Tokens) -> IResult<Partial<Expr>> {
    let neg = map(tokens::op_sub, |_| UnaryOp::Neg);
    let not = map(tokens::op_not, |_| UnaryOp::Not);
    let unary = pair_partial(map(opt(alt((neg, not))), Partial::from), fn_app);
    let expr = map_partial_spanned(unary, |span, (unary, expr)| match unary {
        Some(op) => Expr::from(ExprUnary::new(op, expr, span)),
        None => expr,
    });
    alt((expr, error))(input)
}

fn fn_app(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, first) = project(input)?;
    util::fold_many0(project, first, |lhs, rhs| {
        lhs.flat_map(|lhs| {
            rhs.map(|rhs| {
                let span = Span::merge(lhs.span(), rhs.span());
                Expr::from(ExprFnApp::new(lhs, rhs, span))
            })
        })
    })(input)
}

fn project(input: Tokens) -> IResult<Partial<Expr>> {
    let (input, atomic) = atomic(input)?;

    if let Ok((remaining, _)) = tokens::dot(input) {
        let default = alt((project, error, util::error_expr_if(tokens::eof)));
        let or_default = opt_partial(preceded(tokens::keyword_or, default));

        let (remaining, path) = pair_partial(attr::attr_path, or_default)(remaining)?;
        let proj = atomic.flat_map(|atomic| {
            path.map(|(path, default)| {
                let span = Span::merge(atomic.span(), path.span());
                match default {
                    Some(expr) => Expr::from(ExprProj::new(atomic, path, Some(expr), span)),
                    None => Expr::from(ExprProj::new(atomic, path, None, span)),
                }
            })
        });

        Ok((remaining, proj))
    } else if let Ok((remaining, or_span)) = tokens::keyword_or(input) {
        let expr = atomic.map(|atomic| {
            let arg = Expr::Ident(Ident::from(("or", or_span)));
            let span = Span::merge(atomic.span(), or_span);
            Expr::from(ExprFnApp::new(atomic, arg, span))
        });

        Ok((remaining, expr))
    } else {
        Ok((input, atomic))
    }
}

fn atomic(input: Tokens) -> IResult<Partial<Expr>> {
    let ident = map_partial(atomic::identifier, Expr::from);
    let string = map_partial(atomic::string, Expr::from);
    let literal = map_partial(atomic::literal, Expr::from);
    let inter = map_partial(atomic::interpolation, Expr::from);
    let paren = map_partial(atomic::paren, Expr::from);
    let set = map_partial(atomic::set, Expr::from);
    let list = map_partial(atomic::list, Expr::from);
    let rec_set = map_partial(atomic::rec_set, Expr::from);
    let let_set = map_partial(atomic::let_set, Expr::from);
    alt((
        ident, string, literal, inter, paren, set, list, rec_set, let_set,
    ))(input)
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
