use std::str::{self, FromStr};

use codespan::{ByteIndex, ByteSpan};
use nom::combinator::all_consuming;
use nom::error::ParseError;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom_locate::{self, LocatedSpan};

mod tokens;

type Spanned<'a> = LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<Spanned<'a>, T>;

pub fn parse_expr<S: AsRef<str>>(expr: S) -> Result<(), String> {
    let text = Spanned::new(expr.as_ref());
    unimplemented!()
}

fn map_spanned<'a, O1, O2, F, G>(
    span: Spanned<'a>,
    first: F,
    second: G,
) -> impl Fn(Spanned<'a>) -> IResult<'a, O2>
where
    F: Fn(Spanned<'a>) -> IResult<'a, O1>,
    G: Fn((O1, ByteSpan)) -> O2,
{
    move |input| {
        let byte_span = into_byte_span(span);
        let (input, o1) = first(input)?;
        Ok((input, second((o1, byte_span))))
    }
}

fn into_byte_span(s: Spanned) -> ByteSpan {
    let start = ByteIndex(s.offset as u32);
    let end = ByteIndex(s.offset as u32 + (s.fragment.len().saturating_sub(1) as u32));
    ByteSpan::new(start, end)
}
