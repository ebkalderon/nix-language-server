pub use self::partial::Partial;

use std::str::FromStr;

use codespan::Span;
use nom::combinator::{all_consuming, map, opt};
use nom::error::ErrorKind;
use nom::sequence::{pair, preceded};
use nom::Slice;

use self::error::{Error, Errors};
use self::partial::map_partial;
use crate::ast::{Expr, SourceFile};
use crate::ToSpan;

pub mod error;

mod expr;
mod partial;
mod tokens;

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T, Errors>;

impl<'a> ToSpan for LocatedSpan<'a> {
    fn to_span(&self) -> Span {
        let start = self.offset;
        let end = start + self.fragment.len();
        Span::new(start as u32, end as u32)
    }
}

impl FromStr for Expr {
    type Err = Errors;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_expr(s)
    }
}

impl FromStr for SourceFile {
    type Err = Errors;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_source_file(s)
    }
}

pub fn parse_expr(expr: &str) -> Result<Expr, Errors> {
    parse_expr_partial(expr).and_then(|partial| partial.verify())
}

pub fn parse_expr_partial(expr: &str) -> Result<Partial<Expr>, Errors> {
    let text = LocatedSpan::new(expr);
    match all_consuming(preceded(tokens::space, expr::expr))(text) {
        Ok((_, expr)) => Ok(expr),
        Err(nom::Err::Incomplete(_)) => panic!("file was incomplete"),
        Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => Err(err),
    }
}

pub fn parse_source_file(source: &str) -> Result<SourceFile, Errors> {
    parse_source_file_partial(source).and_then(|partial| partial.verify())
}

pub fn parse_source_file_partial(source: &str) -> Result<Partial<SourceFile>, Errors> {
    let text = LocatedSpan::new(source);
    let comment = preceded(tokens::space_until_final_comment, opt(tokens::comment));
    let expr = map_partial(preceded(tokens::space, expr::expr), Box::new);
    let file = map(pair(comment, expr), |(c, expr)| expr.map(|expr| (c, expr)));
    match all_consuming(map_partial(file, |(c, expr)| SourceFile::new(c, expr)))(text) {
        Ok((_, source)) => Ok(source),
        Err(nom::Err::Incomplete(_)) => panic!("file was incomplete"),
        Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => Err(err),
    }
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

/// Combinator which converts an underlying `nom` error, if any, into a custom parse error.
fn map_err<'a, O, P, F, E>(parser: P, op: F) -> impl Fn(LocatedSpan<'a>) -> IResult<O>
where
    P: Fn(LocatedSpan<'a>) -> IResult<O>,
    F: Fn(Span, &str, ErrorKind) -> E,
    E: Into<Error>,
{
    move |input| match parser(input) {
        Ok(output) => Ok(output),
        Err(nom::Err::Error(mut err)) => {
            if let Some((span, frag, kind)) = err.last().and_then(|e| e.as_nom_error()) {
                let new_err = op(span, frag, kind);
                err.pop();
                err.push(new_err);
            }
            Err(nom::Err::Error(err))
        }
        Err(nom::Err::Failure(mut err)) => {
            if let Some((span, frag, kind)) = err.last().and_then(|e| e.as_nom_error()) {
                let new_err = op(span, frag, kind);
                err.pop();
                err.push(new_err);
            }
            Err(nom::Err::Failure(err))
        }
        Err(err) => Err(err),
    }
}
