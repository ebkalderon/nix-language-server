use std::iter::FromIterator;

use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_nonempty_list;

use super::atomic;
use crate::ast::{AttrPath, AttrSegment};
use crate::lexer::Tokens;
use crate::parser::partial::{map_partial, Partial};
use crate::parser::{tokens, IResult};

pub fn attr_path(input: Tokens) -> IResult<Partial<AttrPath>> {
    let path = separated_nonempty_list(tokens::dot, segment);
    map_partial(map(path, Partial::from_iter), |segs| AttrPath::new(segs))(input)
}

fn segment(input: Tokens) -> IResult<Partial<AttrSegment>> {
    let identifier = map_partial(atomic::identifier, AttrSegment::Ident);
    let string = map_partial(atomic::string, AttrSegment::String);
    alt((identifier, string))(input)
}
