use std::path::PathBuf;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated};

use super::{bind, expr, unary};
use crate::ast::tokens::{Ident, Literal};
use crate::ast::{
    Bind, Expr, ExprInterpolation, ExprLet, ExprList, ExprParen, ExprRec, ExprSet, ExprString,
    StringFragment,
};
use crate::error::{Error, Errors};
use crate::lexer::{StringFragment as LexerFragment, Token, Tokens};
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult};
use crate::ToSpan;

pub fn paren(input: Tokens) -> IResult<Partial<ExprParen>> {
    let expr = terminated(expr, many0(tokens::comment));
    let paren = expect_terminated(preceded(tokens::paren_left, expr), tokens::paren_right);
    map_partial_spanned(paren, |span, inner| ExprParen::new(inner, span))(input)
}

pub fn interpolation(input: Tokens) -> IResult<Partial<ExprInterpolation>> {
    let (remaining, (tokens, span)) = tokens::interpolation(input)?;
    let expr = if tokens.is_empty() {
        let mut errors = Errors::new();
        errors.push(Error::Message(span, "interpolation cannot be empty".into()));
        Partial::with_errors(Some(Expr::Error(span)), errors)
    } else {
        let (_, expr) = expr(Tokens::new(&tokens))?;
        expr
    };

    Ok((remaining, expr.map(|e| ExprInterpolation::new(e, span))))
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

fn set_binds(input: Tokens) -> IResult<Partial<Vec<Bind>>> {
    let term = alt((tokens::brace_right, tokens::semi));
    let binds = many_till_partial(bind::bind, pair(many0(tokens::comment), term));
    let set = terminated(binds, many0(tokens::comment));
    expect_terminated(preceded(tokens::brace_left, set), tokens::brace_right)(input)
}

pub fn list(input: Tokens) -> IResult<Partial<ExprList>> {
    let unary = terminated(unary, many0(tokens::comment));
    let elems = many_till_partial(unary, tokens::bracket_right);
    let inner = preceded(many0(tokens::comment), elems);
    let list = expect_terminated(preceded(tokens::bracket_left, inner), tokens::bracket_right);
    map_partial_spanned(list, |span, exprs| ExprList::new(exprs, span))(input)
}

pub fn string(input: Tokens) -> IResult<Partial<ExprString>> {
    let (remaining, (fragments, span)) = tokens::string(input)?;
    let mut parts = Vec::with_capacity(fragments.len());

    for frag in fragments {
        match frag {
            LexerFragment::Literal(text, span) => {
                parts.push(Partial::from(StringFragment::Literal(text.clone(), *span)));
            }
            LexerFragment::Interpolation(tokens, span) => {
                let expr = if tokens.is_empty() {
                    let mut errors = Errors::new();
                    let message = "interpolation cannot be empty".to_string();
                    errors.push(Error::Message(*span, message));
                    Partial::with_errors(Some(Expr::Error(*span)), errors)
                } else {
                    let (_, expr) = expr(Tokens::new(&tokens))?;
                    expr
                };

                parts.push(expr.map(|expr| {
                    StringFragment::Interpolation(ExprInterpolation::new(expr, *span))
                }));
            }
        }
    }

    let partial: Partial<Vec<_>> = parts.into_iter().collect();
    Ok((remaining, partial.map(|frags| ExprString::new(frags, span))))
}

pub fn literal(input: Tokens) -> IResult<Partial<Literal>> {
    let (remaining, tokens) = take(1usize)(input)?;
    let literal = match tokens.current() {
        Token::Boolean(value, span) => Literal::Boolean(*value, *span),
        Token::Null(span) => Literal::Null(*span),
        Token::Path(value, span) => Literal::Path(PathBuf::from(value.to_string()), *span),
        Token::Float(value, span) => {
            let float: f64 = lexical_core::parse(value.as_bytes()).expect("float parsing failed");
            Literal::Float(float, *span)
        }
        Token::Integer(value, span) => {
            Literal::Integer(value.parse().expect("integer parsing failed"), *span)
        }
        Token::PathTemplate(value, span) => {
            Literal::PathTemplate(PathBuf::from(value.to_string()), *span)
        }
        Token::Uri(value, span) => Literal::Uri(value.parse().expect("URI parsing failed"), *span),
        token => {
            let mut errors = Errors::new();
            errors.push(Error::Nom(token.to_span(), ErrorKind::Tag));
            return Err(nom::Err::Error(errors));
        }
    };

    Ok((remaining, Partial::from(literal)))
}

pub fn identifier(input: Tokens) -> IResult<Partial<Ident>> {
    map(tokens::identifier, Partial::from)(input)
}
