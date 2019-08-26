use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::cut;
use nom::sequence::{pair, preceded, terminated};

use super::{bind, expr, unary};
use crate::ast::{ExprList, ExprParen, ExprSet};
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult, LocatedSpan};

pub fn paren(input: LocatedSpan) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(pair(char('('), tokens::space), expr), char(')'));
    map_partial_spanned(paren, |span, e| ExprParen::new(Box::new(e), span))(input)
}

pub fn set(input: LocatedSpan) -> IResult<Partial<ExprSet>> {
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let binds = many_till_partial(bind, pair(tokens::space, alt((char('}'), char(';')))));
    let set = expect_terminated(preceded(char('{'), binds), pair(tokens::space, char('}')));
    map_partial_spanned(set, |span, binds| ExprSet::new(binds, span))(input)
}

pub fn list(input: LocatedSpan) -> IResult<Partial<ExprList>> {
    let elem = terminated(unary, tokens::space);
    let elems = many_till_partial(elem, alt((char(']'), char(';'))));
    let list = expect_terminated(preceded(pair(char('['), tokens::space), elems), char(']'));
    map_partial_spanned(list, |span, exprs| ExprList::new(exprs, span))(input)
}
