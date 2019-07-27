use std::str;

use nom::bytes::streaming::tag;
use nom::character::streaming::digit1;
use nom::combinator::map;
use nom::multi::length_data;
use nom::sequence::delimited;
use nom::IResult;

pub fn parse_request(input: &str) -> IResult<&str, String> {
    let content_len = delimited(tag("Content-Length: "), digit1, tag("\r\n\r\n"));
    let header = map(content_len, |s: &str| {
        s.parse::<usize>().expect("could not parse")
    });
    let message = length_data(header);
    map(message, |msg| msg.to_string())(input)
}
