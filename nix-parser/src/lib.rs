#![forbid(unsafe_code)]

use codespan::ByteSpan;

pub mod ast;
pub mod parser;

pub trait ToByteSpan {
    fn to_byte_span(&self) -> ByteSpan;
}

impl ToByteSpan for ByteSpan {
    fn to_byte_span(&self) -> ByteSpan {
        *self
    }
}
