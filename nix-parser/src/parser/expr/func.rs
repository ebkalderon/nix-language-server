use codespan::Span;
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};

use super::{expr, util};
use crate::ast::{ExprFnDecl, Formal, Pattern, SetPattern};
use crate::lexer::Tokens;
use crate::parser::partial::{
    map_partial, map_partial_spanned, pair_partial, separated_list_partial, verify_full, Partial,
};
use crate::parser::{tokens, IResult};
use crate::HasSpan;

pub fn fn_decl(input: Tokens) -> IResult<Partial<ExprFnDecl>> {
    let simple = map(map(tokens::identifier, Pattern::Simple), Partial::from);
    let set = map_partial(set_pattern, Pattern::Set);
    let pattern = terminated(alt((simple, set)), tokens::colon);
    let expr = alt((expr, util::error_expr_if(tokens::eof)));
    let fn_decl = map_partial_spanned(pair_partial(pattern, expr), |span, (pattern, expr)| {
        ExprFnDecl::new(pattern, expr, span)
    });
    terminated(fn_decl, many0(tokens::comment))(input)
}

fn set_pattern(input: Tokens) -> IResult<Partial<SetPattern>> {
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
        pair(tokens::brace_left, many0(tokens::comment)),
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

    let sep = pair(tokens::comma, many0(tokens::comment));
    let args = separated_list_partial(sep, tokens::brace_right, formal);
    let start = pair(tokens::brace_left, many0(tokens::comment));
    let term = pair(tokens::brace_right, many0(tokens::comment));

    map_partial_spanned(delimited(start, args, term), |span, formals| {
        SetPattern::new(formals, None, None, span)
    })(input)
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
        set_pattern(tokens).unwrap();
    }
}
