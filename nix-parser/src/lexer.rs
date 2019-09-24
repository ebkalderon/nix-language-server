pub use self::tokens::{StringFragment, Token, Tokens};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::multispace0;
use nom::combinator::{all_consuming, map};
use nom::multi::many0;
use nom::sequence::preceded;

use self::parsers::{comment, identifier, keyword, literal, operator, punctuation, string};
use self::util::check_delims_balanced;
use crate::error::{Error, Errors, UnexpectedError};
use crate::ToSpan;

mod parsers;
mod tokens;
mod util;

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T, Errors>;

#[derive(Clone, Debug, PartialEq)]
pub struct Lexer {
    tokens: Vec<Token>,
    errors: Errors,
}

impl Lexer {
    pub fn new(s: &str) -> Result<Self, Errors> {
        let input = LocatedSpan::new(s);
        match all_consuming(preceded(multispace0, many0(token)))(input) {
            Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => Err(err),
            Err(nom::Err::Incomplete(needed)) => {
                panic!("unable to recover from incomplete input: {:?}", needed)
            }
            Ok((_, tokens)) => {
                let only_comments = tokens.iter().all(|t| t.is_comment());
                let errors = if tokens.is_empty() || only_comments {
                    let mut errors = Errors::new();
                    let message = "Nix expressions cannot be empty".to_string();
                    errors.push(Error::Message(Span::initial(), message));
                    return Err(errors);
                } else {
                    check_delims_balanced(&tokens)
                };

                Ok(Lexer { tokens, errors })
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
        comment,
        operator,
        string,
        punctuation,
        literal,
        keyword,
        identifier,
        unknown,
    ))(input)
}

fn unknown(input: LocatedSpan) -> IResult<Token> {
    map(take(1usize), |span: LocatedSpan| {
        let error = UnexpectedError::new(format!("token `{}`", span.fragment), span.to_span());
        Token::Unknown(span.fragment.into(), span.to_span(), error.into())
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer() {
        let lexer = Lexer::new(
            r#"
{
  foo = ''
    hello
    ${}
    world
  '';
}"#,
        )
        .unwrap();
        println!("{:?}", lexer.tokens());
    }
}
