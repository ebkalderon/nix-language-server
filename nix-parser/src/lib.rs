#![forbid(unsafe_code)]

use codespan::ByteSpan;

pub mod ast;
pub mod parser;

pub trait ToByteSpan {
    fn to_byte_span(&self) -> ByteSpan;
}
