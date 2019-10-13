use codespan::Span;
use nom::combinator::{peek, recognize};

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
