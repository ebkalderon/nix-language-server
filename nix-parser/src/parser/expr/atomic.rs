use std::iter::FromIterator;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::char;
use nom::combinator::{cut, map, map_parser, recognize};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::{many0, many_till};
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{bind, expr};
use crate::ast::{ExprParen, ExprSet};
use crate::parser::partial::{map_partial, partial_or, Partial};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn paren(input: Span) -> IResult<Partial<ExprParen>> {
    let paren = delimited(pair(char('('), tokens::space), expr, char(')'));
    let valid = map_partial(paren, |e| ExprParen::new(Box::new(e), input.to_byte_span()));

    let invalid = map(preceded(pair(char('('), tokens::space), expr), |mut expr| {
        let err = VerboseError {
            errors: vec![(input, VerboseErrorKind::Char(')'))],
        };
        expr.extend_errors(err);
        expr.map(|expr| ExprParen::new(Box::new(expr), input.to_byte_span()))
    });

    alt((valid, invalid))(input)
}

pub fn set(input: Span) -> IResult<Partial<ExprSet>> {
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let close_brace = pair(tokens::space, char('}'));
    let binds = map(many_till(bind, close_brace), |(p, _)| Partial::from_iter(p));
    let set = preceded(pair(char('{'), tokens::space_until_final_comment), binds);
    map_partial(set, |binds| ExprSet::new(binds, input.to_byte_span()))(input)
}
