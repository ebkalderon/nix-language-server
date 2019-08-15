use codespan::{ByteIndex, ByteSpan};
use nom::combinator::all_consuming;
use nom::error::VerboseError;
use nom::sequence::{preceded, terminated};
use nom_locate::LocatedSpan;

use self::expr::bind;
use self::partial::{many1_partial, verify_full, Partial};
use crate::ast::{Bind, SourceFile};
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

pub fn parse_expr_partial<'a>(expr: &'a str) -> Result<Partial<'a, Vec<Bind>>, String> {
    let text = Span::new(expr);
    let expr = many1_partial(";", preceded(tokens::space_until_final_comment, bind::bind));
    all_consuming(terminated(expr, tokens::space))(text)
        .map(|(_, binds)| binds)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_expr<'a>(expr: &'a str) -> Result<Vec<Bind>, String> {
    let text = Span::new(expr);
    let expr = many1_partial(";", preceded(tokens::space_until_final_comment, bind::bind));
    all_consuming(terminated(verify_full(expr), tokens::space))(text)
        .map(|(_, binds)| binds)
        .map_err(|e| format!("{:?}", e))
}

pub fn parse_source_file<'a>(source: &'a str) -> Result<SourceFile, VerboseError<Span<'a>>> {
    let _text = Span::new(source);
    unimplemented!()
}
