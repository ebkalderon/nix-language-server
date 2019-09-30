use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::multi::many0;
use nom::sequence::terminated;

use super::{expr, util};
use crate::ast::tokens::Ident;
use crate::ast::{ExprFnDecl, FnDeclFormals, FnDeclSimple, Formal};
use crate::error::{Errors, ExpectedFoundError};
use crate::lexer::Tokens;
use crate::parser::partial::{map_partial, pair_partial, Partial};
use crate::parser::{tokens, IResult};
use crate::{HasSpan, ToSpan};

pub fn fn_decl(input: Tokens) -> IResult<Partial<ExprFnDecl>> {
    let simple = map_partial(simple, ExprFnDecl::Simple);
    terminated(simple, many0(tokens::comment))(input)
}

fn simple(input: Tokens) -> IResult<Partial<FnDeclSimple>> {
    let expr = alt((expr, util::error_expr_if(tokens::eof, "<eof>")));
    map_partial(pair_partial(identifier_arg, expr), |(ident, body)| {
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
