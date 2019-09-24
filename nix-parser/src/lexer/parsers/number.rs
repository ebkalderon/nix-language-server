use std::str::FromStr;

use nom::bytes::complete::{take, take_while};
use nom::character::complete::{char, digit0, digit1, one_of};
use nom::combinator::{cut, map_parser, map_res, opt, recognize};
use nom::error::ErrorKind;
use nom::sequence::{pair, tuple};
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
        Regex::from_str(r#"(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?"#).unwrap()
    });

    if let Some(m) = regex.captures(input.fragment).and_then(|c| c.get(0)) {
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

    #[test]
    fn float_literals() {
        let string = LocatedSpan::new("1.23");
        let (_, bare_frac_float) = all_consuming(float)(string).expect("bare float failed");
        match bare_frac_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 1.23)),
            token => panic!("bare float produced token: {:?}", token),
        }

        let string = LocatedSpan::new("6.1E4");
        let (_, bare_exp_float) = all_consuming(float)(string).expect("bare exp float failed");
        match bare_exp_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 6.1E4)),
            token => panic!("bare exp float produced token: {:?}", token),
        }

        let string = LocatedSpan::new("12.0E-3");
        let (_, neg_exp_float) = all_consuming(float)(string).expect("negative exp float failed");
        match neg_exp_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 12.0E-3)),
            token => panic!("negative exp float produced token: {:?}", token),
        }

        let string = LocatedSpan::new("6.9E+5");
        let (_, pos_exp_float) = all_consuming(float)(string).expect("positive exp float failed");
        match pos_exp_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 6.9E+5)),
            token => panic!("positive exp float produced token: {:?}", token),
        }

        let string = LocatedSpan::new("44.3e5");
        let (_, lower_exp_float) = all_consuming(float)(string).expect("lower exp float failed");
        match lower_exp_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 44.3e5)),
            token => panic!("lower exp float produced token: {:?}", token),
        }

        let string = LocatedSpan::new(".123");
        let (_, opt_zero_float) = all_consuming(float)(string).expect("optional zero float failed");
        match opt_zero_float {
            Token::Float(value, _) => assert!(approx_eq!(f64, value, 0.123)),
            token => panic!("optional zero float produced token: {:?}", token),
        }
    }

    #[test]
    fn integer_literals() {
        let string = LocatedSpan::new("123");
        let (_, bare_integer) = all_consuming(integer)(string).expect("integer failed");
        match bare_integer {
            Token::Integer(value, _) => assert_eq!(value, 123),
            token => panic!("integer produced token: {:?}", token),
        }
    }
}
