pub use self::partial::Partial;

use std::str::FromStr;

use codespan::{ByteIndex, ByteSpan};
use nom::combinator::all_consuming;
use nom::error::VerboseError;
use nom::sequence::preceded;
use nom_locate::LocatedSpan;

use self::partial::verify_full;
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

pub fn parse_expr_partial(expr: &str) -> Result<Partial<Expr>, String> {
    let text = Span::new(expr);
    all_consuming(preceded(tokens::space, expr::expr))(text)
        .map(|(_, binds)| binds)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_expr(expr: &str) -> Result<Expr, String> {
    let text = Span::new(expr);
    all_consuming(preceded(tokens::space, verify_full(expr::expr)))(text)
        .map(|(_, binds)| binds)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_source_file<'a>(source: &'a str) -> Result<SourceFile, VerboseError<Span<'a>>> {
    let _text = Span::new(source);
    unimplemented!()
}
