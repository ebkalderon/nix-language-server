use nom::character::complete::char;
use nom::sequence::{pair, preceded, tuple};

use super::{bind, expr};
use crate::ast::{ExprAssert, ExprLetIn, ExprWith};
use crate::parser::partial::{
    expect_terminated, many_till_partial, map_partial, map_partial_spanned, pair_partial, Partial,
};
use crate::parser::{tokens, IResult, LocatedSpan};

pub fn with(input: LocatedSpan) -> IResult<Partial<ExprWith>> {
    let keyword = pair(tokens::keyword_with, tokens::space);
    let with = expect_terminated(preceded(keyword, expr), char(';'));
    let body = preceded(tokens::space, expr);
    let stmt = pair_partial(map_partial(with, Box::new), map_partial(body, Box::new));
    map_partial_spanned(stmt, |span, (with, body)| ExprWith::new(with, body, span))(input)
}

pub fn assert(input: LocatedSpan) -> IResult<Partial<ExprAssert>> {
    let keyword = pair(tokens::keyword_assert, tokens::space);
    let cond = expect_terminated(preceded(keyword, expr), char(';'));
    let body = preceded(tokens::space, expr);
    let stmt = pair_partial(map_partial(cond, Box::new), map_partial(body, Box::new));
    map_partial_spanned(stmt, |span, (cond, body)| ExprAssert::new(cond, body, span))(input)
}

pub fn let_in(input: LocatedSpan) -> IResult<Partial<ExprLetIn>> {
    let keyword_let = pair(tokens::keyword_let, tokens::space1);
    let keyword_in = pair(tokens::space, tokens::keyword_in);
    let bind = preceded(tokens::space_until_final_comment, bind::bind);
    let binds = preceded(keyword_let, many_till_partial(bind, keyword_in));

    let keyword_in = tuple((tokens::space, tokens::keyword_in, tokens::space1));
    let body = map_partial(expr, Box::new);
    let stmt = pair_partial(expect_terminated(binds, keyword_in), body);
    map_partial_spanned(stmt, |span, (binds, body)| {
        ExprLetIn::new(binds, body, span)
    })(input)
}
