use codespan::Span;
use nom::bytes::complete::take;

use super::IResult;
use crate::ast::tokens::{Comment, Ident, Literal};
use crate::error::{Error, Errors, ExpectedFoundError};
use crate::lexer::{StringFragment, Token, Tokens};
use crate::ToSpan;

macro_rules! define_tokens {
    ($($function:ident { $($inner:tt)+ })+) => {
        $(define_tokens!(@token $function { $($inner)+ });)+
    };

    (@token $function:ident { $variant:ident, $expects:expr }) => {
        define_tokens!(@token $function {
            returns: Span,
            parse: Token::$variant(ref span) => *span,
            expects: $expects,
        });
    };

    (@token $function:ident { returns: $ret:ty, parse: $variant:pat => $value:expr, expects: $expects:expr, }) => {
        pub fn $function(input: Tokens) -> IResult<$ret> {
            let (remaining, tokens) = take(1usize)(input)?;
            match tokens.current() {
                $variant => Ok((remaining, $value)),
                token => {
                    let mut errors = Errors::new();
                    let found = token.description();
                    errors.push(ExpectedFoundError::new($expects, found, token.to_span()));
                    Err(nom::Err::Error(errors))
                }
            }
        }
    };
}

define_tokens! {
    eof { Eof, "<eof>" }
    unknown {
        returns: (String, Span, Error),
        parse: Token::Unknown(ref text, ref span, ref err) => (text.clone(), *span, err.clone()),
        expects: "unknown token",
    }

    comment {
        returns: Comment,
        parse: Token::Comment(ref text, ref span) => Comment::from((text.clone(), *span)),
        expects: "comment",
    }
    identifier {
        returns: Ident,
        parse: Token::Identifier(ref ident, ref span) => Ident::from((ident.clone(), *span)),
        expects: "identifier",
    }
    null {
        returns: Literal,
        parse: Token::Null(ref span) => Literal::from(((), *span)),
        expects: "null",
    }
    boolean {
        returns: Literal,
        parse: Token::Boolean(ref value, ref span) => Literal::from((*value, *span)),
        expects: "boolean",
    }
    float {
        returns: Literal,
        parse: Token::Float(ref value, ref span) => Literal::from((*value, *span)),
        expects: "floating-point number",
    }

    integer {
        returns: Literal,
        parse: Token::Integer(ref value, ref span) => Literal::from((*value, *span)),
        expects: "integer",
    }
    // interpolation => { Interpolation, "interpolation" },
    path {
        returns: Literal,
        parse: Token::Path(ref value, ref span) => Literal::from((value.as_path(), *span)),
        expects: "path literal",
    }
    path_template {
        returns: Literal,
        parse: Token::PathTemplate(ref value, ref span) => Literal::PathTemplate(value.clone(), *span),
        expects: "path template",
    }
    string {
        returns: (Vec<StringFragment>, Span),
        parse: Token::String(ref frags, ref span) => (frags.clone(), *span),
        expects: "string",
    }
    uri {
        returns: Literal,
        parse: Token::Uri(ref value, ref span) => Literal::from((value.clone(), *span)),
        expects: "URI",
    }

    op_add { Add, "operator `+`" }
    op_sub { Sub, "operator `-`" }
    op_mul { Mul, "operator `*`" }
    op_div { Div, "operator `/`" }
    op_eq { IsEq, "operator `==`" }
    op_neq { NotEq, "operator `!=`" }
    op_lt { LessThan, "operator `<`" }
    op_lte { LessThanEq, "operator `<=`" }
    op_gt { GreaterThan, "operator `>`" }
    op_gte { GreaterThanEq, "operator `>=`" }
    op_and { LogicalAnd, "operator `&&`" }
    op_or { LogicalOr, "operator `||`" }
    op_concat { Concat, "operator `++`" }
    op_update { Update, "operator `//`" }
    op_question { Question, "operator `?`" }
    op_imply { Imply, "operator `->`" }
    op_not { Not, "unary operator `!`" }

    keyword_assert { Assert, "keyword `assert`" }
    keyword_else { Else, "keyword `else`" }
    keyword_if { If, "keyword `if`" }
    keyword_in { In, "keyword `in`" }
    keyword_inherit { Inherit, "keyword `inherit`" }
    keyword_let { Let, "keyword `let`" }
    keyword_or { Or, "keyword `or`" }
    keyword_rec { Rec, "keyword `rec`" }
    keyword_then { Then, "keyword `then`" }
    keyword_with { With, "keyword `with`" }

    at { At, "at symbol (`@`)" }
    colon { Colon, "colon" }
    comma { Comma, "comma" }
    ellipsis { Ellipsis, "ellipsis (`...`)" }
    dot { Dot, "dot separator" }
    eq { Eq, "equals sign" }
    interpolate { Interpolate, "interpolation sign (`${}`)" }
    brace_left { LBrace, "left brace" }
    brace_right { RBrace, "right brace" }
    bracket_left { LBracket, "left bracket" }
    bracket_right { RBracket, "right bracket" }
    paren_left { LParen, "left parentheses" }
    paren_right { RParen, "right parentheses" }
    quote_double { QuoteDouble, "double quote" }
    quote_single { QuoteSingle, "multiline string open (`''`)" }
    semi { Semi, "semicolon" }
}
