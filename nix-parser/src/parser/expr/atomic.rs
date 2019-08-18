use std::iter::FromIterator;

use nom::character::complete::char;
use nom::combinator::{cut, map, map_parser, recognize};
use nom::multi::{many0, many_till};
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{bind, expr};
use crate::ast::{ExprParen, ExprSet};
use crate::parser::partial::{
    many0_partial, map_partial, partial, partial_until, verify_full, Partial,
};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn paren(input: Span) -> IResult<Partial<ExprParen>> {
    let paren = delimited(pair(char('('), tokens::space), expr, cut(char(')')));
    map_partial(paren, |e| ExprParen::new(Box::new(e), input.to_byte_span()))(input)
}

pub fn set(input: Span) -> IResult<Partial<ExprSet>> {
    // 1. Works correctly in all cases, but isn't partial.
    //
    // let binds = map(
    //     many0(preceded(tokens::space_until_final_comment, bind::bind)),
    //     Partial::from,
    // );
    // let set = delimited(char('{'), terminated(binds, tokens::space), cut(char('}')));

    // 2. Is partial, but only supports empty sets.
    //
    let binds = map(
        many_till(
            preceded(tokens::space_until_final_comment, partial(bind::bind)),
            pair(tokens::space, char('}')),
        ),
        |(p, _)| Partial::from_iter(p),
    );
    let set = preceded(pair(char('{'), tokens::space_until_final_comment), binds);

    // 3. Original, is partial, and does not support nested sets of any kind.
    //
    // let binds = many0_partial(";", preceded(tokens::space_until_final_comment, bind::bind));
    // let set = delimited(char('{'), terminated(binds, tokens::space), cut(char('}')));

    map_partial(set, |binds| ExprSet::new(binds, input.to_byte_span()))(input)
}
