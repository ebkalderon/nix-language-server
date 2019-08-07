use codespan::{ByteIndex, ByteSpan};
use nom_locate::LocatedSpan;

mod tokens;

type Span<'a> = LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<Span<'a>, T>;

pub fn parse_expr<S: AsRef<str>>(expr: S) -> Result<(), String> {
    let _text = Span::new(expr.as_ref());
    unimplemented!()
}

fn map_spanned<'a, O1, O2, F, G>(
    span: Span<'a>,
    first: F,
    second: G,
) -> impl Fn(Span<'a>) -> IResult<O2>
where
    F: Fn(Span<'a>) -> IResult<O1>,
    G: Fn((O1, ByteSpan)) -> O2,
{
    move |input| {
        let byte_span = into_byte_span(span);
        let (input, o1) = first(input)?;
        Ok((input, second((o1, byte_span))))
    }
}

fn into_byte_span(s: Span) -> ByteSpan {
    let start = s.offset;
    let end = start + s.fragment.len().saturating_sub(1);
    ByteSpan::new(ByteIndex(start as u32), ByteIndex(end as u32))
}
