use std::str::FromStr;

use nom::character::complete::digit1;
use nom::combinator::map_res;
use nom::error::ErrorKind;
use nom::sequence::terminated;
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use crate::error::{Error, Errors};
use crate::lexer::util::map_spanned;
use crate::lexer::{IResult, LocatedSpan, Token};
use crate::ToSpan;

pub fn float(input: LocatedSpan) -> IResult<Token> {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| {
        Regex::from_str(r#"\A(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?"#).unwrap()
    });

    if let Some(m) = regex.find(input.fragment) {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);
        let float = span.fragment.parse().expect("float parsing cannot fail");
        Ok((remaining, Token::Float(float, span.to_span())))
    } else {
        let mut errors = Errors::new();
        errors.push(Error::Nom(
            input.to_span(),
            input.fragment.into(),
            ErrorKind::Float,
        ));
        Err(nom::Err::Error(errors))
    }
}

pub fn integer(input: LocatedSpan) -> IResult<Token> {
    let int = map_res(digit1, |s: LocatedSpan| i64::from_str(s.fragment));
    map_spanned(int, |span, value| Token::Integer(value, span))(input)
}

#[cfg(test)]
mod tests {
    use float_cmp::approx_eq;
    use nom::combinator::all_consuming;

    use super::*;

    fn assert_float_eq(string: &str, expected: f64) {
        let span = LocatedSpan::new(string);
        match all_consuming(float)(span) {
            Ok((_, Token::Float(value, _))) => assert!(approx_eq!(f64, value, expected)),
            Ok((_, token)) => panic!("parsing float {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing float {:?} failed: {:?}", string, err),
        }
    }

    fn assert_integer_eq(string: &str, expected: i64) {
        let span = LocatedSpan::new(string);
        match all_consuming(integer)(span) {
            Ok((_, Token::Integer(value, _))) => assert_eq!(value, expected),
            Ok((_, token)) => panic!("parsing integer {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing integer {:?} failed: {:?}", string, err),
        }
    }

    #[test]
    fn float_literals() {
        assert_float_eq("1.23", 1.23);
        assert_float_eq("6.1E4", 6.1E4);
        assert_float_eq("12.0E-3", 12.0E-3);
        assert_float_eq("6.9E+5", 6.9E+5);
        assert_float_eq("44.3e5", 44.3e5);
        assert_float_eq(".123", 0.123);
    }

    #[test]
    fn integer_literals() {
        assert_integer_eq("123", 123);
        assert_integer_eq("00001", 1);
    }
}
