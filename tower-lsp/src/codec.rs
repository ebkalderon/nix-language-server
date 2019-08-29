use std::error::Error;
use std::io::{self, Write};
use std::str;

use bytes::{BufMut, BytesMut};
use nom::bytes::streaming::tag;
use nom::character::streaming::digit1;
use nom::combinator::{map, map_res};
use nom::multi::length_data;
use nom::sequence::delimited;
use nom::{Err, IResult, Needed};
use tokio::codec::{Decoder, Encoder};

pub type ParseError = Box<dyn Error + Send + Sync>;

#[derive(Clone, Debug, Default)]
pub struct LanguageServerCodec {
    remaining_msg_bytes: usize,
}

impl Encoder for LanguageServerCodec {
    type Item = String;
    type Error = ParseError;

    fn encode(&mut self, item: Self::Item, dst: &mut BytesMut) -> Result<(), Self::Error> {
        if !item.is_empty() {
            dst.reserve(item.len() + 30);
            let mut writer = dst.writer();
            write!(writer, "Content-Length: {}\r\n\r\n{}", item.len(), item)?;
            writer.flush()?;
        }

        Ok(())
    }
}

impl Decoder for LanguageServerCodec {
    type Item = String;
    type Error = ParseError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if self.remaining_msg_bytes > src.len() {
            return Ok(None);
        }

        let string = str::from_utf8(src)?;
        let (message, len) = match parse_request(string) {
            Ok((remaining, message)) => (message.to_string(), src.len() - remaining.len()),
            Err(Err::Incomplete(Needed::Size(min))) => {
                self.remaining_msg_bytes = min;
                return Ok(None);
            }
            Err(Err::Incomplete(_)) => {
                return Ok(None);
            }
            Err(err) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("parsing '{}' resulted in error: {:?}", string, err),
                ))?
            }
        };

        src.advance(len);
        self.remaining_msg_bytes = 0;

        Ok(Some(message))
    }
}

fn parse_request(input: &str) -> IResult<&str, String> {
    let content_len = delimited(tag("Content-Length: "), digit1, tag("\r\n\r\n"));
    let header = map_res(content_len, |s: &str| s.parse::<usize>());
    let message = length_data(header);
    map(message, |msg| msg.to_string())(input)
}
