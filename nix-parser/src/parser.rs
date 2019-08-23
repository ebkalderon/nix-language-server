pub use self::partial::Partial;

use std::str::FromStr;

use codespan::Span;
use nom::combinator::{all_consuming, map, opt};
use nom::error::VerboseError;
use nom::sequence::{pair, preceded};
use nom::Slice;

use self::partial::map_partial;
use crate::ast::{Expr, SourceFile};
use crate::ToSpan;

mod expr;
mod partial;
mod tokens;

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T, VerboseError<LocatedSpan<'a>>>;

impl<'a> ToSpan for LocatedSpan<'a> {
    fn to_span(&self) -> Span {
        let start = self.offset;
        let end = start + self.fragment.len().saturating_sub(1);
        Span::new(start as u32, end as u32)
    }
}

impl FromStr for Expr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_expr(s)
    }
}

impl FromStr for SourceFile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_source_file(s)
    }
}

pub fn parse_expr(expr: &str) -> Result<Expr, String> {
    parse_expr_partial(expr).and_then(|partial| partial.verify().map_err(|e| format!("{:?}", e)))
}

pub fn parse_expr_partial(expr: &str) -> Result<Partial<Expr>, String> {
    let text = LocatedSpan::new(expr);
    all_consuming(preceded(tokens::space, expr::expr))(text)
        .map(|(_, expr)| expr)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_source_file(source: &str) -> Result<SourceFile, String> {
    parse_source_file_partial(source)
        .and_then(|partial| partial.verify().map_err(|e| format!("{:?}", e)))
}

pub fn parse_source_file_partial(source: &str) -> Result<Partial<SourceFile>, String> {
    let text = LocatedSpan::new(source);
    let comment = preceded(tokens::space_until_final_comment, opt(tokens::comment));
    let expr = map_partial(preceded(tokens::space, expr::expr), Box::new);
    let file = map(pair(comment, expr), |(c, expr)| expr.map(|expr| (c, expr)));
    all_consuming(map_partial(file, |(c, expr)| SourceFile::new(c, expr)))(text)
        .map(|(_, source)| source)
        .map_err(|e| format!("{:?}", e))
}

/// Combinator which behaves like `nom::combinator::map()`, except it also includes a `ByteSpan`
/// based on the consumed input.
fn map_spanned<'a, O1, O2, P, F>(parser: P, f: F) -> impl Fn(LocatedSpan<'a>) -> IResult<O2>
where
    P: Fn(LocatedSpan<'a>) -> IResult<O1>,
    F: Fn(Span, O1) -> O2,
{
    move |input| {
        let (remainder, value) = parser(input)?;
        let value_len = remainder.offset - input.offset;
        let span = input.slice(..value_len).to_span();
        Ok((remainder, f(span, value)))
    }
}
