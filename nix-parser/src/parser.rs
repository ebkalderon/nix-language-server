pub use self::partial::Partial;

use std::str::FromStr;

use codespan::Span;
use nom::combinator::{all_consuming, map, opt};
use nom::sequence::terminated;

use self::partial::{map_partial, pair_partial};
use crate::ast::{Expr, SourceFile};
use crate::error::Errors;
use crate::lexer::{Lexer, Tokens};
use crate::ToSpan;

mod expr;
mod partial;
mod tokens;

type IResult<'a, T> = nom::IResult<Tokens<'a>, T, Errors>;

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
    let lexer = Lexer::new(expr)?;
    let tokens = lexer.tokens();
    let errors = lexer.errors().clone();

    let mut partial = match all_consuming(terminated(expr::expr, tokens::eof))(tokens) {
        Ok((_, partial)) => partial,
        Err(nom::Err::Incomplete(_)) => panic!("file was incomplete"),
        Err(nom::Err::Error(mut err)) | Err(nom::Err::Failure(mut err)) => {
            err.extend(errors);
            return Err(err);
        }
    };

    partial.extend_errors(errors);
    Ok(partial)
}

pub fn parse_source_file(source: &str) -> Result<SourceFile, Errors> {
    parse_source_file_partial(source).and_then(|partial| partial.verify())
}

pub fn parse_source_file_partial(source: &str) -> Result<Partial<SourceFile>, Errors> {
    let lexer = Lexer::new(source)?;
    let tokens = lexer.tokens();
    let errors = lexer.errors().clone();

    let comment = map(opt(tokens::comment), Partial::from);
    let expr = map_partial(expr::expr, Box::new);
    let source_file = map_partial(pair_partial(comment, expr), |(c, e)| SourceFile::new(c, e));
    let mut partial = match all_consuming(terminated(source_file, tokens::eof))(tokens) {
        Ok((_, partial)) => partial,
        Err(nom::Err::Incomplete(_)) => panic!("file was incomplete"),
        Err(nom::Err::Error(mut err)) | Err(nom::Err::Failure(mut err)) => {
            err.extend(errors);
            return Err(err);
        }
    };

    partial.extend_errors(errors);
    Ok(partial)
}

/// Combinator which behaves like `nom::combinator::map()`, except it also includes a `ByteSpan`
/// based on the consumed input.
fn map_spanned<'a, O1, O2, P, F>(parser: P, f: F) -> impl Fn(Tokens<'a>) -> IResult<O2>
where
    P: Fn(Tokens<'a>) -> IResult<O1>,
    F: Fn(Span, O1) -> O2,
{
    move |input| {
        let (remainder, value) = parser(input)?;
        let span = Span::new(input.to_span().start(), remainder.to_span().start());
        Ok((remainder, f(span, value)))
    }
}
