pub use self::number::{float, integer};
pub use self::path::{path, path_template};

use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::map;

use crate::ast::tokens::Literal;
use crate::parser::{IResult, Span};
use crate::ToByteSpan;

mod number;
mod path;

pub fn literal(input: Span) -> IResult<Literal> {
    let boolean = map(boolean, |b| Literal::from((b, input)));
    let float = map(float, |f| Literal::from((f, input)));
    let integer = map(integer, |i| Literal::from((i, input)));
    let null = map(null, |n| Literal::from((n, input)));
    let path = map(path, |p| Literal::from((p, input)));
    let temp = map(path_template, |t| {
        Literal::PathTemplate(t, input.to_byte_span())
    });
    alt((boolean, float, integer, null, path, temp))(input)
}

pub fn boolean(input: Span) -> IResult<bool> {
    let true_val = map(tag("true"), |_| true);
    let false_val = map(tag("false"), |_| false);
    alt((true_val, false_val))(input)
}

pub fn null(input: Span) -> IResult<()> {
    map(tag("null"), |_| ())(input)
}

fn take_n(n: usize) -> impl Fn(Span) -> IResult<Span> {
    move |input| take(n)(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn boolean_literal() {
        let string = Span::new("true");
        let (_, true_val) = all_consuming(boolean)(string).unwrap();
        assert_eq!(true_val, true);

        let string = Span::new("false");
        let (_, false_val) = all_consuming(boolean)(string).unwrap();
        assert_eq!(false_val, false);
    }

    #[test]
    fn null_literal() {
        let string = Span::new("null");
        let (_, null) = all_consuming(null)(string).unwrap();
        assert_eq!(null, ());
    }
}
