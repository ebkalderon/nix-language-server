pub use self::tokens::{CommentKind, StringFragment, Token, Tokens};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::multispace0;
use nom::combinator::{all_consuming, map};
use nom::multi::many0;
use nom::sequence::{preceded, terminated};

use self::lexers::{comment, identifier, interpolation, literal, operator, punctuation, string};
use self::util::check_delims_balanced;
use crate::error::{Error, Errors, UnexpectedError};
use crate::ToSpan;

mod lexers;
mod tokens;
mod util;

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

impl<'a> ToSpan for LocatedSpan<'a> {
    fn to_span(&self) -> Span {
        let start = self.offset;
        let end = start + self.fragment.len();
        Span::new(start as u32, end as u32)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lexer<'a> {
    tokens: Vec<Token<'a>>,
    errors: Errors,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Result<Self, Errors> {
        let input = LocatedSpan::new(s);
        let tokens = many0(terminated(token, multispace0));
        match all_consuming(preceded(multispace0, tokens))(input) {
            Ok((_, tokens)) => {
                let (mut tokens, mut errors) = filter_unexpected_tokens(tokens);

                let end = input.fragment.len().saturating_sub(1) as u32;
                let eof_span = Span::new(end, end);

                let only_comments = tokens.iter().all(|t| t.is_comment());
                let errors = if tokens.is_empty() || only_comments {
                    let message = "Nix expressions must resolve to a value".to_string();
                    errors.push(Error::Message(Span::initial(), message));
                    return Err(errors);
                } else {
                    errors.extend(check_delims_balanced(&tokens, eof_span));
                    errors
                };

                tokens.push(Token::Eof(eof_span));
                Ok(Lexer { tokens, errors })
            }
            Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
                let (input, kind) = err;
                let mut errors = Errors::new();
                errors.push(Error::Nom(input.to_span(), kind));
                Err(errors)
            }
            Err(nom::Err::Incomplete(needed)) => {
                let mut errors = Errors::new();
                let message = format!("unable to recover from incomplete input: {:?}", needed);
                errors.push(Error::Message(Span::initial(), message));
                Err(errors)
            }
        }
    }

    pub fn tokens(&self) -> Tokens<'_> {
        Tokens::new(self.tokens.as_slice())
    }

    pub fn errors(&self) -> &Errors {
        &self.errors
    }
}

fn token(input: LocatedSpan) -> IResult<Token> {
    alt((
        literal,
        identifier,
        string,
        interpolation,
        comment,
        punctuation,
        operator,
        unknown,
    ))(input)
}

fn unknown(input: LocatedSpan) -> IResult<Token> {
    map(take(1usize), |span: LocatedSpan| {
        let error = UnexpectedError::new(format!("`{}`", span.fragment), span.to_span());
        Token::Unknown(span.fragment.into(), span.to_span(), error.into())
    })(input)
}

fn filter_unexpected_tokens(tokens: Vec<Token>) -> (Vec<Token>, Errors) {
    // FIXME: Replace this with `Vec::drain_filter()` once stabilized.
    let (invalid, valid): (Vec<_>, _) = tokens.into_iter().partition(|token| match token {
        Token::Unknown(..) => true,
        _ => false,
    });
    let errors: Errors = invalid
        .into_iter()
        .filter_map(|token| match token {
            Token::Unknown(_, _, error) => Some(error),
            _ => None,
        })
        .collect();
    (valid, errors)
}
