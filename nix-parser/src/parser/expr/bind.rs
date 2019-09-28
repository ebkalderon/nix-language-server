use codespan::Span;
use nom::branch::alt;
use nom::combinator::{map, peek};
use nom::multi::many0;
use nom::sequence::preceded;

use super::{expr, util};
use crate::ast::tokens::{Comment, Ident, IdentPath};
use crate::ast::{Bind, BindInherit, BindInheritExpr, BindSimple, Expr};
use crate::error::{Error, Errors, ExpectedFoundError};
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
    expect_terminated(alt((inherit_expr, inherit, simple)), tokens::semi)(input)
}

fn simple(input: Tokens) -> IResult<Partial<BindSimple>> {
    let attr = map(tokens::identifier, |ident| {
        Partial::from(IdentPath::from((vec![ident.clone()], ident.span())))
    });
    let found = "one of `;` or `}`";
    let error = util::error_expr_if(peek(alt((tokens::semi, tokens::brace_right))), found);

    let lhs = expect_terminated(attr, tokens::eq);
    let rhs = map_partial(alt((expr, error)), Box::new);
    let bind = pair_partial(final_comment, pair_partial(lhs, rhs));

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
    let inner = alt((expr, util::error_expr_if(peek(tokens::paren_right), "`}`")));
    let expr = expect_terminated(preceded(tokens::paren_left, inner), tokens::paren_right);
    let bind = preceded(tokens::keyword_inherit, pair_partial(expr, ident_sequence));
    map_partial_spanned(bind, |span, (expr, idents)| {
        BindInheritExpr::new(Box::new(expr), idents, span)
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
