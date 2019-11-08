use std::iter::FromIterator;

use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_nonempty_list;

use super::{atomic, expr};
use crate::ast::{AttrPath, AttrSegment, Expr, ExprInterpolation};
use crate::error::{Error, Errors};
use crate::lexer::Tokens;
use crate::parser::partial::{map_partial, Partial};
use crate::parser::{tokens, IResult};

pub fn attr_path(input: Tokens) -> IResult<Partial<AttrPath>> {
    let path = separated_nonempty_list(tokens::dot, segment);
    map_partial(map(path, Partial::from_iter), AttrPath::new)(input)
}

fn segment(input: Tokens) -> IResult<Partial<AttrSegment>> {
    let identifier = map_partial(atomic::identifier, AttrSegment::Ident);
    let string = map_partial(atomic::string, AttrSegment::String);
    let interpolation = map_partial(interpolation, AttrSegment::Interpolation);
    alt((identifier, string, interpolation))(input)
}

fn interpolation(input: Tokens) -> IResult<Partial<ExprInterpolation>> {
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
