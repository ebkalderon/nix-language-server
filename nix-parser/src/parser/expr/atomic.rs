use nom::branch::alt;
use nom::character::complete::char;
use nom::sequence::{pair, preceded, terminated};

use super::{bind, expr, unary};
use crate::ast::{Bind, ExprLet, ExprList, ExprParen, ExprRec, ExprSet};
use crate::parser::partial::{expect_terminated, many_till_partial, map_partial_spanned, Partial};
use crate::parser::{tokens, IResult, LocatedSpan};

pub fn paren(input: LocatedSpan) -> IResult<Partial<ExprParen>> {
    let paren = expect_terminated(preceded(pair(char('('), tokens::space), expr), char(')'));
    map_partial_spanned(paren, |span, e| ExprParen::new(Box::new(e), span))(input)
}

pub fn set(input: LocatedSpan) -> IResult<Partial<ExprSet>> {
    map_partial_spanned(set_binds, |span, binds| ExprSet::new(binds, span))(input)
}

pub fn rec_set(input: LocatedSpan) -> IResult<Partial<ExprRec>> {
    let keyword_rec = pair(tokens::keyword_rec, tokens::space);
    let rec_set = preceded(keyword_rec, set_binds);
    map_partial_spanned(rec_set, |span, binds| ExprRec::new(binds, span))(input)
}

pub fn let_set(input: LocatedSpan) -> IResult<Partial<ExprLet>> {
    let keyword_let = pair(tokens::keyword_let, tokens::space);
    let let_set = preceded(keyword_let, set_binds);
    map_partial_spanned(let_set, |span, binds| ExprLet::new(binds, span))(input)
}

pub fn list(input: LocatedSpan) -> IResult<Partial<ExprList>> {
    let elem = terminated(unary, tokens::space);
    let elems = many_till_partial(elem, alt((char(']'), char(';'))));
    let list = expect_terminated(preceded(pair(char('['), tokens::space), elems), char(']'));
    map_partial_spanned(list, |span, exprs| ExprList::new(exprs, span))(input)
}

fn set_binds(input: LocatedSpan) -> IResult<Partial<Vec<Bind>>> {
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let binds = many_till_partial(bind, pair(tokens::space, alt((char('}'), char(';')))));
    expect_terminated(preceded(char('{'), binds), pair(tokens::space, char('}')))(input)
}
