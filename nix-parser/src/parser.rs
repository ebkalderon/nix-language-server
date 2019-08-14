use codespan::{ByteIndex, ByteSpan};
use nom::character::complete::multispace0;
use nom::combinator::all_consuming;
use nom::error::VerboseError;
use nom::multi::many1;
use nom::sequence::{preceded, terminated};
use nom_locate::LocatedSpan;

use crate::ast::{Bind, SourceFile};
use crate::ToByteSpan;

mod expr;
mod tokens;

type Span<'a> = LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<Span<'a>, T, VerboseError<Span<'a>>>;

impl<'a> ToByteSpan for Span<'a> {
    fn to_byte_span(&self) -> ByteSpan {
        let start = self.offset;
        let end = start + self.fragment.len().saturating_sub(1);
        ByteSpan::new(ByteIndex(start as u32), ByteIndex(end as u32))
    }
}

pub fn parse_expr<S: AsRef<str>>(expr: S) -> Result<Vec<Bind>, String> {
    let text = Span::new(expr.as_ref());
    let expr = many1(terminated(expr::bind::bind, multispace0));
    all_consuming(preceded(tokens::space, expr))(text)
        .map(|(_, bind)| bind)
        .map_err(|err| format!("{:?}", err))
}

pub fn parse_source_file<'a>(source: &'a str) -> Result<SourceFile, VerboseError<Span<'a>>> {
    let _text = Span::new(source);
    unimplemented!()
}
