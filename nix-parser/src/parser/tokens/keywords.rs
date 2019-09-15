use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alphanumeric1;
use nom::combinator::{not, peek};
use nom::sequence::terminated;

use crate::parser::error::ExpectedFoundError;
use crate::parser::{map_err, IResult, LocatedSpan};

macro_rules! define_keywords {
    ($($function:ident => $keyword:tt),+) => {
        pub fn is_keyword(input: &LocatedSpan) -> bool {
            match input.fragment {
                $(stringify!($keyword))|+ => true,
                _ => false
            }
        }

        pub fn keyword(input: LocatedSpan) -> IResult<LocatedSpan> {
            let terms = alt(($($function),+));
            terminated(terms, peek(not(alphanumeric1)))(input)
        }

        $(
            pub fn $function(input: LocatedSpan) -> IResult<LocatedSpan> {
                map_err(tag(stringify!($keyword)), |span, frag, _| {
                    let found = if frag.chars().next().filter(|c| c.is_alphabetic()).is_some() {
                        frag.split_whitespace().next().map(|s| format!("`{}`", s)).unwrap()
                    } else {
                        frag.chars().next().map(|c| format!("`{}`", c)).unwrap_or_else(|| "EOF".into())
                    };

                    ExpectedFoundError::new(
                        vec![format!("keyword `{}`", stringify!($keyword))],
                        found,
                        span,
                    )
                })(input)
            }
        )+
    };
}

define_keywords! {
    keyword_assert => assert,
    keyword_else => else,
    keyword_if => if,
    keyword_in => in,
    keyword_inherit => inherit,
    keyword_let => let,
    keyword_or => or,
    keyword_rec => rec,
    keyword_then => then,
    keyword_with => with
}
