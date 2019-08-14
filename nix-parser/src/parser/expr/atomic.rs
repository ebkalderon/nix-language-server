use nom::character::complete::char;
use nom::combinator::map;
use nom::sequence::{delimited, pair, terminated};

use super::expr;
use crate::ast::ExprParen;
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn paren(input: Span) -> IResult<ExprParen> {
    let paren = delimited(pair(char('('), tokens::space), expr, char(')'));
    map(paren, |e| ExprParen::new(Box::new(e), input.to_byte_span()))(input)
}
