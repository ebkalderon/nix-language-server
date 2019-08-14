use std::str::FromStr;

use codespan::{ByteIndex, ByteSpan};
use nom::bytes::complete::take_until;
use nom::character::complete::{char, multispace0};
use nom::combinator::{all_consuming, map_parser, peek, recognize};
use nom::error::VerboseError;
use nom::multi::separated_nonempty_list;
use nom::sequence::{pair, preceded, terminated};
use nom_locate::LocatedSpan;

use crate::ast::{Bind, Expr, SourceFile};
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

pub fn parse_expr<'a>(expr: &'a str) -> Result<Vec<Bind>, String> {
    let text = Span::new(expr);
    let bind = map_parser(
        recognize(pair(take_until(";"), char(';'))),
        expr::bind::bind,
    );
    let expr = separated_nonempty_list(multispace0, all_consuming(terminated(bind, multispace0)));
    preceded(tokens::space, expr)(text)
        .map(|(_, bind)| bind)
        .map_err(|err| format!("{:?}", err))
}

pub fn parse_source_file<'a>(source: &'a str) -> Result<SourceFile, VerboseError<Span<'a>>> {
    let _text = Span::new(source);
    unimplemented!()
}
