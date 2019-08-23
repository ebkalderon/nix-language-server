use std::str::FromStr;

use nom::character::complete::{char, digit0, digit1, one_of};
use nom::combinator::{map_res, opt, recognize};
use nom::sequence::{pair, tuple};

use crate::parser::{IResult, LocatedSpan};

pub fn float(input: LocatedSpan) -> IResult<f64> {
    let frac = pair(char('.'), digit1);
    let exp = tuple((one_of("eE"), opt(one_of("+-")), digit1));
    let number = tuple((opt(char('-')), digit0, frac, opt(exp)));
    let float = recognize(number);
    map_res(float, |s: LocatedSpan| f64::from_str(s.fragment))(input)
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

        let string = LocatedSpan::new("-2.5");
        let (_, neg_frac_float) = all_consuming(float)(string).expect("negative float failed");
        assert!(approx_eq!(f64, neg_frac_float, -2.5));

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
