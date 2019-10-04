use codespan::Span;
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::preceded;

use super::{attr, expr, util};
use crate::ast::tokens::{Comment, Ident};
use crate::ast::{Bind, BindInherit, BindInheritExpr, BindSimple};
use crate::error::{Error, Errors, UnexpectedError};
use crate::lexer::Tokens;
use crate::parser::partial::{
    expect_terminated, map_partial, map_partial_spanned, pair_partial, Partial,
};
use crate::parser::{tokens, IResult};
use crate::{HasSpan, ToSpan};

pub fn bind(input: Tokens) -> IResult<Partial<Bind>> {
    let inherit_expr = map_partial(inherit_expr, Bind::InheritExpr);
    let inherit = map_partial(inherit, Bind::Inherit);
    let simple = map_partial(simple, Bind::Simple);
    match expect_terminated(alt((inherit_expr, inherit, simple)), tokens::semi)(input) {
        Ok(output) => Ok(output),
        Err(_) => {
            let mut errors = Errors::new();
            let description = input.current().description();
            let span = input.current().to_span();
            errors.push(UnexpectedError::new(description, span));
            Err(nom::Err::Error(errors))
        }
    }
}

fn simple(input: Tokens) -> IResult<Partial<BindSimple>> {
    let found = "one of `;` or `}`";
    let error = util::error_expr_if(alt((tokens::semi, tokens::brace_right)), found);
    let lhs = expect_terminated(attr::attr_path, tokens::eq);
    let bind = pair_partial(final_comment, pair_partial(lhs, alt((expr, error))));

    map_partial(bind, move |(comment, (attr, expr))| {
        let span = Span::merge(attr.span(), expr.span());
        BindSimple::new(comment, attr, expr, span)
    })(input)
}

fn inherit(input: Tokens) -> IResult<Partial<BindInherit>> {
    let bind = preceded(tokens::keyword_inherit, ident_sequence);
    map_partial_spanned(bind, |span, idents| BindInherit::new(idents, span))(input)
}

fn inherit_expr(input: Tokens) -> IResult<Partial<BindInheritExpr>> {
    let inner = alt((expr, util::error_expr_if(tokens::paren_right, "`}`")));
    let expr = expect_terminated(preceded(tokens::paren_left, inner), tokens::paren_right);
    let bind = preceded(tokens::keyword_inherit, pair_partial(expr, ident_sequence));
    map_partial_spanned(bind, |span, (expr, idents)| {
        BindInheritExpr::new(expr, idents, span)
    })(input)
}

fn final_comment(input: Tokens) -> IResult<Partial<Option<Comment>>> {
    map(many0(tokens::comment), |mut comments| {
        Partial::new(Some(comments.pop()))
    })(input)
}

fn ident_sequence(input: Tokens) -> IResult<Partial<Vec<Ident>>> {
    let (remaining, idents) = many0(tokens::identifier)(input)?;
    if idents.is_empty() {
        let mut errors = Errors::new();
        let span = input.current().to_span();
        let message = "expected at least one identifier".to_string();
        errors.push(Error::Message(span, message));
        Ok((remaining, Partial::with_errors(None, errors)))
    } else {
        Ok((remaining, Partial::from(idents)))
    }
}
