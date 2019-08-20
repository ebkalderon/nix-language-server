pub use self::partial::Partial;

use std::str::FromStr;

use codespan::{ByteIndex, ByteSpan};
use nom::combinator::{all_consuming, map, opt};
use nom::error::VerboseError;
use nom::sequence::{pair, preceded};
use nom::Slice;
use nom_locate::LocatedSpan;

use self::partial::{map_partial, verify_full};
use crate::ast::{Expr, SourceFile};
use crate::ToByteSpan;

mod expr;
mod partial;
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

impl FromStr for Expr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_expr(s)
    }
}

pub fn parse_expr(expr: &str) -> Result<Expr, String> {
    let text = Span::new(expr);
    all_consuming(preceded(tokens::space, verify_full(expr::expr)))(text)
        .map(|(_, expr)| expr)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_expr_partial(expr: &str) -> Result<Partial<Expr>, String> {
    let text = Span::new(expr);
    all_consuming(preceded(tokens::space, expr::expr))(text)
        .map(|(_, expr)| expr)
        .map_err(|e| format!("{:?}", e))
}

impl FromStr for SourceFile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_source_file(s)
    }
}

pub fn parse_source_file(source: &str) -> Result<SourceFile, String> {
    let text = Span::new(source);
    let comment = preceded(tokens::space_until_final_comment, opt(tokens::comment));
    let expr = map(preceded(tokens::space, verify_full(expr::expr)), Box::new);
    let file = pair(comment, expr);
    all_consuming(map(file, |(comment, expr)| SourceFile::new(comment, expr)))(text)
        .map(|(_, source)| source)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_source_file_partial(source: &str) -> Result<Partial<SourceFile>, String> {
    let text = Span::new(source);
    let comment = preceded(tokens::space_until_final_comment, opt(tokens::comment));
    let expr = map_partial(preceded(tokens::space, expr::expr), Box::new);
    let file = map(pair(comment, expr), |(c, expr)| expr.map(|e| (c, e)));
    all_consuming(map_partial(file, |(c, e)| SourceFile::new(c, e)))(text)
        .map(|(_, source)| source)
        .map_err(|e| format!("{:?}", e))
}

fn map_spanned<'a, O1, O2, P, F>(
    input: Span<'a>,
    parser: P,
    f: F,
) -> impl Fn(Span<'a>) -> IResult<O2>
where
    P: Fn(Span<'a>) -> IResult<O1>,
    F: Fn(ByteSpan, O1) -> O2,
{
    move |input| {
        let (remainder, value) = parser(input)?;
        let value_len = remainder.offset - input.offset;
        let span = input.slice(..value_len).to_byte_span();
        Ok((remainder, f(span, value)))
    }
}
