use nom::branch::alt;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated};

use super::{bind, expr, unary, util};
use crate::ast::tokens::{Ident, Literal};
use crate::ast::{
    Bind, ExprInterpolation, ExprLet, ExprList, ExprParen, ExprRec, ExprSet, ExprString,
    StringFragment,
};
use crate::lexer::{StringFragment as LexerFragment, Tokens};
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult};

pub fn paren(input: Tokens) -> IResult<Partial<ExprParen>> {
    let expr = terminated(expr, many0(tokens::comment));
    let paren = expect_terminated(preceded(tokens::paren_left, expr), tokens::paren_right);
    map_partial_spanned(paren, |span, inner| ExprParen::new(inner, span))(input)
}

pub fn interpolation(input: Tokens) -> IResult<Partial<ExprInterpolation>> {
    let (remaining, (tokens, span)) = tokens::interpolation(input)?;
    let error = util::error_expr_if(tokens::brace_right, "right brace");
    let (_, expr) = alt((expr, error))(Tokens::new(&tokens))?;
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
                let error = util::error_expr_if(tokens::brace_right, "right brace");
                let (_, expr) = alt((expr, error))(Tokens::new(tokens))?;
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
    let boolean = map(tokens::boolean, Partial::from);
    let null = map(tokens::null, Partial::from);
    let path = map(tokens::path, Partial::from);
    let float = map(tokens::float, Partial::from);
    let integer = map(tokens::integer, Partial::from);
    let path_template = map(tokens::path_template, Partial::from);
    let uri = map(tokens::uri, Partial::from);
    alt((boolean, null, float, integer, path, path_template, uri))(input)
}

pub fn identifier(input: Tokens) -> IResult<Partial<Ident>> {
    map(tokens::identifier, Partial::from)(input)
}
