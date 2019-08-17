use nom::character::complete::char;
use nom::combinator::{cut, map};
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{bind, expr};
use crate::ast::{ExprParen, ExprSet};
use crate::parser::partial::{many0_partial, map_partial, verify_full, Partial};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn paren(input: Span) -> IResult<ExprParen> {
    let expr = verify_full(expr);
    let paren = delimited(pair(char('('), tokens::space), expr, cut(char(')')));
    map(paren, |e| ExprParen::new(Box::new(e), input.to_byte_span()))(input)
}

pub fn set(input: Span) -> IResult<Partial<ExprSet>> {
    let binds = many0_partial(";", preceded(tokens::space_until_final_comment, bind::bind));
    let set = delimited(char('{'), terminated(binds, tokens::space), cut(char('}')));
    map_partial(set, |binds| ExprSet::new(binds, input.to_byte_span()))(input)
}
