use nom::branch::alt;
use nom::combinator::{map, peek};
use nom::multi::many0;

use super::expr;
use crate::ast::tokens::{Comment, IdentPath};
use crate::ast::{Bind, BindSimple, Expr};
use crate::error::{Errors, ExpectedFoundError};
use crate::lexer::Tokens;
use crate::parser::partial::{
    expect_terminated, map_partial, map_partial_spanned, pair_partial, Partial,
};
use crate::parser::{tokens, IResult};
use crate::{HasSpan, ToSpan};

pub fn bind(input: Tokens) -> IResult<Partial<Bind>> {
    expect_terminated(map_partial(simple, Bind::Simple), tokens::semi)(input)
}

fn simple(input: Tokens) -> IResult<Partial<BindSimple>> {
    let attr = map(tokens::identifier, |ident| {
        Partial::from(IdentPath::from((vec![ident.clone()], ident.span())))
    });
    let error = error_expr_if(peek(alt((tokens::semi, tokens::brace_right))));

    let lhs = expect_terminated(attr, tokens::eq);
    let rhs = map_partial(alt((expr, error)), Box::new);
    let bind = pair_partial(final_comment, pair_partial(lhs, rhs));

    map_partial_spanned(bind, move |span, (comment, (attr, expr))| {
        BindSimple::new(comment, attr, expr, span)
    })(input)
}

fn final_comment(input: Tokens) -> IResult<Partial<Option<Comment>>> {
    map(many0(tokens::comment), |mut comments| {
        Partial::new(Some(comments.pop()))
    })(input)
}

fn error_expr_if<'a, O, F>(parser: F) -> impl Fn(Tokens<'a>) -> IResult<Partial<Expr>>
where
    F: Fn(Tokens<'a>) -> IResult<O>,
    O: ToSpan,
{
    move |input| match parser(input) {
        Err(error) => Err(error),
        Ok((remaining, token)) => {
            let mut errors = Errors::new();
            errors.push(ExpectedFoundError::new(
                "expression",
                "token",
                token.to_span(),
            ));
            let expr = Expr::Error(token.to_span());
            let partial = Partial::with_errors(Some(expr), errors);
            Ok((remaining, partial))
        }
    }
}
