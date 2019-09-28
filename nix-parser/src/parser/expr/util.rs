use nom::combinator::peek;

use crate::ast::Expr;
use crate::error::{Errors, ExpectedFoundError};
use crate::lexer::{Token, Tokens};
use crate::parser::partial::Partial;
use crate::parser::IResult;
use crate::ToSpan;

pub fn error_expr_if<'a, O, F>(
    parser: F,
    found: &'a str,
) -> impl Fn(Tokens<'a>) -> IResult<Partial<Expr>>
where
    F: Fn(Tokens<'a>) -> IResult<O>,
    O: ToSpan,
{
    move |input| match peek(&parser)(input) {
        Err(error) => Err(error),
        Ok((remaining, token)) => {
            let span = token.to_span();
            let mut errors = Errors::new();
            errors.push(ExpectedFoundError::new("expression", found, span));
            let expr = Partial::with_errors(Some(Expr::Error(span)), errors);
            Ok((remaining, expr))
        }
    }
}
