use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::sequence::terminated;

use super::{attr, expr, util};
use crate::ast::tokens::{Comment, Ident};
use crate::ast::{ExprFnDecl, FnDeclFormals, FnDeclSimple, Formal};
use crate::error::{Error, Errors, ExpectedFoundError};
use crate::lexer::Tokens;
use crate::parser::partial::{expect_terminated, map_partial, pair_partial, Partial};
use crate::parser::{tokens, IResult};
use crate::{HasSpan, ToSpan};

pub fn fn_decl(input: Tokens) -> IResult<Partial<ExprFnDecl>> {
    map_partial(simple, ExprFnDecl::Simple)(input)
}

fn simple(input: Tokens) -> IResult<Partial<FnDeclSimple>> {
    let expr = alt((expr, util::error_expr_if(tokens::eof, "<eof>")));
    let simple = pair_partial(identifier_arg, map_partial(expr, Box::new));
    map_partial(simple, |(ident, body)| {
        let span = Span::merge(ident.span(), body.span());
        FnDeclSimple::new(ident, body, span)
    })(input)
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
