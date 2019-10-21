use codespan::Span;
use nom::combinator::{peek, recognize};
use nom::error::{ErrorKind, ParseError};

use crate::ast::Expr;
use crate::error::{Errors, ExpectedFoundError};
use crate::lexer::Tokens;
use crate::parser::partial::Partial;
use crate::parser::IResult;
use crate::ToSpan;

pub fn error_expr_if<'a, F>(token: F) -> impl Fn(Tokens<'a>) -> IResult<Partial<Expr>>
where
    F: Fn(Tokens<'a>) -> IResult<Span>,
{
    move |input| match peek(recognize(&token))(input) {
        Err(error) => Err(error),
        Ok((remaining, tokens)) => {
            let desc = tokens.current().description();
            let span = tokens.current().to_span();
            let mut errors = Errors::new();
            errors.push(ExpectedFoundError::new("expression", desc, span));
            let expr = Partial::with_errors(Some(Expr::Error(span)), errors);
            Ok((remaining, expr))
        }
    }
}

/// Vendored version of `nom::multi::fold_many0` except it doesn't require `R: Clone`.
///
/// This combinator should yield noticeably higher performance for complex types, but is not
/// compatible with other `nom` combinators expecting `Fn` closures.
pub fn fold_many0<I, O, E, F, G, R>(f: F, init: R, g: G) -> impl FnOnce(I) -> nom::IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: Fn(I) -> nom::IResult<I, O, E>,
    G: Fn(R, O) -> R,
    E: ParseError<I>,
{
    move |i: I| {
        let mut res = init;
        let mut input = i.clone();

        loop {
            let i_ = input.clone();
            match f(i_) {
                Ok((i, o)) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if i == input {
                        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Many0)));
                    }

                    res = g(res, o);
                    input = i;
                }
                Err(nom::Err::Error(_)) => {
                    return Ok((input, res));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
}
