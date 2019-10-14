use nom::branch::alt;
use nom::character::complete::{char, digit0, digit1, one_of};
use nom::combinator::{map, opt, recognize};
use nom::sequence::{pair, tuple};

use crate::lexer::util::map_spanned;
use crate::lexer::{IResult, LocatedSpan, Token};
use crate::ToSpan;

#[cfg_attr(feature = "profile", flamer::flame)]
pub fn float(input: LocatedSpan) -> IResult<Token> {
    let first = recognize(tuple((one_of("123456789"), digit0, char('.'), digit0)));
    let second = recognize(tuple((opt(char('0')), char('.'), digit1)));
    let suffix = opt(tuple((one_of("Ee"), opt(one_of("+-")), digit1)));
    let float = recognize(pair(alt((first, second)), suffix));
    map(float, |span: LocatedSpan| {
        Token::Float(span.fragment.into(), span.to_span())
    })(input)
}

#[cfg_attr(feature = "profile", flamer::flame)]
pub fn integer(input: LocatedSpan) -> IResult<Token> {
    map_spanned(digit1, |span, value| {
        Token::Integer(value.fragment.into(), span)
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    fn assert_float_eq(string: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(float)(span) {
            Ok((_, Token::Float(value, _))) => assert_eq!(value, string),
            Ok((_, token)) => panic!("parsing float {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing float {:?} failed: {:?}", string, err),
        }
    }

    fn assert_integer_eq(string: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(integer)(span) {
            Ok((_, Token::Integer(value, _))) => assert_eq!(value, string),
            Ok((_, token)) => panic!("parsing integer {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing integer {:?} failed: {:?}", string, err),
        }
    }

    #[test]
    fn float_literals() {
        assert_float_eq("1.23");
        assert_float_eq("6.1E4");
        assert_float_eq("12.0E-3");
        assert_float_eq("6.9E+5");
        assert_float_eq("44.3e5");
        assert_float_eq(".123");
    }

    #[test]
    fn integer_literals() {
        assert_integer_eq("123");
        assert_integer_eq("00001");
    }
}
