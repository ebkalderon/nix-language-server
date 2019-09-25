use std::str::FromStr;

use nom::bytes::complete::take_while;
use nom::character::complete::{char, digit0, digit1, one_of};
use nom::combinator::{cut, map_res, opt, recognize};
use nom::sequence::{pair, tuple};
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use crate::parser::error::{Errors, ExpectedFoundError};
use crate::parser::{IResult, LocatedSpan};

pub fn float(input: LocatedSpan) -> IResult<f64> {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| {
        Regex::from_str(r#"(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?"#).unwrap()
    });

    if let Some(m) = regex.captures(input.fragment).and_then(|c| c.get(0)) {
        let span = input.slice(m.start()..m.end());
        let remaining = input.slice(m.end()..);
        let float = span.fragment.parse().expect("float parsing cannot fail");
        Ok((remaining, float))
    } else {
        let (_, token) = take_while(|c: char| !" \n,;=)}".contains(c))(input)?;
        let mut errors = Errors::new();
        errors.push(ExpectedFoundError::new(
            "float",
            format!("`{}`", token.fragment),
            token,
        ));
        Err(nom::Err::Error(errors))
    }
}

pub fn integer(input: LocatedSpan) -> IResult<i64> {
    let int = recognize(pair(opt(char('-')), digit1));
    map_res(int, |s: LocatedSpan| i64::from_str(s.fragment))(input)
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
        assert!(approx_eq!(f64, bare_frac_float, 1.23));

        let string = LocatedSpan::new("6.1E4");
        let (_, bare_exp_float) = all_consuming(float)(string).expect("bare exp float failed");
        assert!(approx_eq!(f64, bare_exp_float, 6.1E4));

        let string = LocatedSpan::new("12.0E-3");
        let (_, neg_exp_float) = all_consuming(float)(string).expect("negative exp float failed");
        assert!(approx_eq!(f64, neg_exp_float, 12.0E-3));

        let string = LocatedSpan::new("6.9E+5");
        let (_, pos_exp_float) = all_consuming(float)(string).expect("positive exp float failed");
        assert!(approx_eq!(f64, pos_exp_float, 6.9E+5));

        let string = LocatedSpan::new("44.3e5");
        let (_, lower_exp_float) = all_consuming(float)(string).expect("lower exp float failed");
        assert!(approx_eq!(f64, lower_exp_float, 44.3e5));

        let string = LocatedSpan::new(".123");
        let (_, opt_zero_float) = all_consuming(float)(string).expect("optional zero float failed");
        assert!(approx_eq!(f64, opt_zero_float, 0.123));
    }

    #[test]
    fn integer_literals() {
        let string = LocatedSpan::new("123");
        let (_, bare_integer) = all_consuming(integer)(string).expect("bare integer failed");
        assert_eq!(bare_integer, 123);

        let string = LocatedSpan::new("-42");
        let (_, neg_integer) = all_consuming(integer)(string).expect("negative integer failed");
        assert_eq!(neg_integer, -42);
    }
}
