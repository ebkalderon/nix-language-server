use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{expr, util::error_expr_if};
use crate::ast::tokens::Ident;
use crate::ast::{ExprFnDecl, FnDeclFormals, FnDeclSimple, Formal};
use crate::error::{Errors, ExpectedFoundError};
use crate::lexer::Tokens;
use crate::parser::partial::{
    map_partial, map_partial_spanned, pair_partial, separated_list_partial, verify_full, Partial,
};
use crate::parser::{tokens, IResult};
use crate::{HasSpan, ToSpan};

pub fn fn_decl(input: Tokens) -> IResult<Partial<ExprFnDecl>> {
    let simple = map_partial(simple, ExprFnDecl::Simple);
    let formals = map_partial(formals, ExprFnDecl::Formals);
    terminated(alt((simple, formals)), many0(tokens::comment))(input)
}

fn simple(input: Tokens) -> IResult<Partial<FnDeclSimple>> {
    let expr = alt((expr, error_expr_if(tokens::eof, "<eof>")));
    map_partial(pair_partial(identifier_arg, expr), |(ident, body)| {
        let span = Span::merge(ident.span(), body.span());
        FnDeclSimple::new(ident, body, span)
    })(input)
}

fn formals(input: Tokens) -> IResult<Partial<FnDeclFormals>> {
    let value = alt((expr, error_expr_if(tokens::comma, "comma")));
    let default = opt(preceded(tokens::op_question, verify_full(value)));
    let formal = map(pair(tokens::identifier, default), |(name, def)| {
        let name_span = name.span();
        let default_span = def.as_ref().map(|d| d.span()).unwrap_or(name_span);
        Partial::from(Formal::new(name, def, Span::merge(name_span, default_span)))
    });

    // overloading trailing comma
    let ellipsis = alt((
        map(tokens::comma, |_| None),
        map(preceded(tokens::comma, tokens::ellipsis), |ellipsis| {
            Some(ellipsis)
        }),
    ));
    let args = pair_partial(
        separated_list_partial(tokens::comma, tokens::brace_right, formal),
        map(opt(ellipsis), |e: Option<_>| {
            Partial::from(e.and_then(std::convert::identity))
        }),
    );
    let term = pair(tokens::brace_right, tokens::colon);
    let formals = delimited(tokens::brace_left, args, term);

    let expr = alt((expr, error_expr_if(tokens::eof, "<eof>")));
    map_partial_spanned(
        pair_partial(formals, expr),
        |span, ((formals, ellipsis), expr)| FnDeclFormals::new(formals, ellipsis, None, expr, span),
    )(input)
}

fn identifier_arg(input: Tokens) -> IResult<Partial<Ident>> {
    if let Ok((remaining, ident)) = terminated(tokens::identifier, tokens::colon)(input) {
        Ok((remaining, Partial::from(ident)))
    } else {
        let (remaining, tokens) = terminated(take(1usize), tokens::colon)(input)?;
        let found = tokens.current().description();
        let span = tokens.current().to_span();
        let mut errors = Errors::new();
        errors.push(ExpectedFoundError::new("identifier", found, span));
        Ok((remaining, Partial::with_errors(None, errors)))
    }
}
