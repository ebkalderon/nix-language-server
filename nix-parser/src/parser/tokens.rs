use codespan::Span;
use nom::bytes::complete::take;
use nom::error::ErrorKind;

use super::IResult;
use crate::ast::tokens::{Comment, Ident};
use crate::error::{Error, Errors, ExpectedFoundError};
use crate::lexer::{StringFragment, Token, Tokens};
use crate::ToSpan;

pub fn comment(input: Tokens) -> IResult<Comment> {
    let (remaining, tokens) = take(1usize)(input)?;
    match tokens.current() {
        Token::Comment(text, _, span) => Ok((remaining, Comment::from((text.as_str(), *span)))),
        token => {
            let mut errors = Errors::new();
            errors.push(Error::Nom(token.to_span(), ErrorKind::Tag));
            Err(nom::Err::Error(errors))
        }
    }
}

pub fn identifier(input: Tokens) -> IResult<Ident> {
    let (remaining, tokens) = take(1usize)(input)?;
    match tokens.current() {
        Token::Identifier(text, span) => Ok((remaining, Ident::from((text.to_string(), *span)))),
        token => {
            let mut errors = Errors::new();
            let expected = "identifier";
            let found = token.description();
            errors.push(ExpectedFoundError::new(expected, found, token.to_span()));
            Err(nom::Err::Error(errors))
        }
    }
}

pub fn interpolation(input: Tokens) -> IResult<(&[Token], Span)> {
    let (remaining, tokens) = take(1usize)(input)?;
    match tokens.current() {
        Token::Interpolation(tokens, span) => Ok((remaining, (&tokens[..], *span))),
        token => {
            let mut errors = Errors::new();
            errors.push(Error::Nom(token.to_span(), ErrorKind::Tag));
            Err(nom::Err::Error(errors))
        }
    }
}

pub fn string(input: Tokens) -> IResult<(&[StringFragment], Span)> {
    let (remaining, tokens) = take(1usize)(input)?;
    match tokens.current() {
        Token::String(ref frags, ref span) => Ok((remaining, (&frags[..], *span))),
        token => {
            let mut errors = Errors::new();
            errors.push(Error::Nom(token.to_span(), ErrorKind::Tag));
            Err(nom::Err::Error(errors))
        }
    }
}

macro_rules! define_simple_tokens {
    ($($function:ident => $name:ident $(( $expected:expr ))? ),+) => {
        $(define_simple_tokens!(@token $function => $name $(($expected))?);)+
    };

    (@token $function:ident => $variant:ident) => {
        pub fn $function(input: Tokens) -> IResult<Span> {
            let (remaining, tokens) = take(1usize)(input)?;
            match tokens.current() {
                Token::$variant(span) => Ok((remaining, *span)),
                token => {
                    let mut errors = Errors::new();
                    errors.push(Error::Nom(token.to_span(), ErrorKind::Tag));
                    Err(nom::Err::Error(errors))
                }
            }
        }
    };

    (@token $function:ident => $variant:ident ( $expected:expr )) => {
        pub fn $function(input: Tokens) -> IResult<Span> {
            let (remaining, tokens) = take(1usize)(input)?;
            match tokens.current() {
                Token::$variant(span) => Ok((remaining, *span)),
                token => {
                    let mut errors = Errors::new();
                    let found = token.description();
                    errors.push(ExpectedFoundError::new($expected, found, token.to_span()));
                    Err(nom::Err::Error(errors))
                }
            }
        }
    };
}

define_simple_tokens! {
    eof => Eof("<eof>"),

    keyword_assert => Assert("keyword `assert`"),
    keyword_else => Else("keyword `else`"),
    keyword_if => If,
    keyword_in => In("keyword `in`"),
    keyword_inherit => Inherit("keyword `inherit`"),
    keyword_let => Let("keyword `let`"),
    keyword_or => Or,
    keyword_rec => Rec,
    keyword_then => Then("keyword `then`"),
    keyword_with => With("keyword `with`"),

    op_add => Add,
    op_sub => Sub,
    op_mul => Mul,
    op_div => Div,
    op_eq => IsEq,
    op_neq => NotEq,
    op_lt => LessThan,
    op_lte => LessThanEq,
    op_gt => GreaterThan,
    op_gte => GreaterThanEq,
    op_and => LogicalAnd,
    op_or => LogicalOr,
    op_concat => Concat,
    op_update => Update,
    op_question => Question,
    op_imply => Imply,
    op_not => Not,

    at => At("at symbol (`@`)"),
    colon => Colon("colon"),
    comma => Comma("comma"),
    ellipsis => Ellipsis("ellipsis (`...`)"),
    dot => Dot("dot separator"),
    eq => Eq("equals sign"),
    brace_left => LBrace,
    brace_right => RBrace("right brace"),
    bracket_left => LBracket,
    bracket_right => RBracket("right bracket"),
    paren_left => LParen,
    paren_right => RParen("right parentheses"),
    semi => Semi("semicolon")
}
