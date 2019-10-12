use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{expr, util};
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
    let expr = alt((expr, util::error_expr_if(tokens::eof)));
    map_partial(pair_partial(identifier_arg, expr), |(ident, body)| {
        let span = Span::merge(ident.span(), body.span());
        FnDeclSimple::new(ident, body, span)
    })(input)
}

fn formals(input: Tokens) -> IResult<Partial<FnDeclFormals>> {
    // Definition:
    // a leading token sequence B of a non-terminal A is a production rule of B
    // such that if a non-terminal C satisfies the condition that all words in its
    // language L(C) has a prefix as a word in L(B), then L(A) = L(C), ie. A and C
    // produces the same set of words.
    // Note: unfortunately computing a leading token sequence of any non-terminal in any grammar
    // is undecidable, but luckily we know one for this.

    // Quickly look for a leading token sequence, because recovery with partial
    // parsing result may produces invalid parsing tree on the rest of the tokens
    // if there is no sequence of valid leading tokens

    // According to the grammar, the leading token sequence will be
    // left-curly-bracket ( right-curly-bracket colon / ellipsis / ident ( question / comma / right-curly-bracket ))
    fn ignore<T>(_: T) {}
    preceded(
        tokens::brace_left,
        alt((
            map(preceded(tokens::brace_right, tokens::colon), ignore),
            map(tokens::ellipsis, ignore),
            preceded(
                tokens::identifier,
                alt((
                    map(tokens::op_question, ignore),
                    map(tokens::comma, ignore),
                    map(tokens::brace_right, ignore),
                )),
            ),
        )),
    )(input)?;

    let value = alt((expr, util::error_expr_if(tokens::comma)));
    let default = opt(preceded(tokens::op_question, verify_full(value)));
    let formal = map(pair(tokens::identifier, default), |(name, def)| {
        let name_span = name.span();
        let default_span = def.as_ref().map(|d| d.span()).unwrap_or(name_span);
        Partial::from(Formal::new(name, def, Span::merge(name_span, default_span)))
    });

    let args = separated_list_partial(tokens::comma, tokens::brace_right, formal);
    let term = pair(tokens::brace_right, tokens::colon);
    let formals = delimited(tokens::brace_left, args, term);

    let expr = alt((expr, util::error_expr_if(tokens::eof)));
    map_partial_spanned(pair_partial(formals, expr), |span, (formals, expr)| {
        FnDeclFormals::new(formals, None, None, expr, span)
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;

    #[test]
    fn formal_args() {
        let source = r#"{foo}:foo"#;
        let lexer = Lexer::new(source).unwrap();
        let tokens = lexer.tokens();
        formals(tokens).unwrap();
    }
}
