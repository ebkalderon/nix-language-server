//! Utilities for lexing and tokenizing.

pub use self::tokens::{CommentKind, StringFragment, StringKind, Token, Tokens};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::character::complete::multispace1;
use nom::combinator::{iterator, map};
use nom::error::ErrorKind;

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

/// Converts an input string into a sequence of tokens.
///
/// # Examples
///
/// ```
/// # use nix_parser::error::Errors;
/// # fn main() -> Result<(), Errors> {
/// use nix_parser::lexer::Lexer;
///
/// let expr = r#"
///     { foo, bar, ... }:
///     with import <nixpkgs> {};
///     let
///         inherit foo;
///         baz = { quux = 12 + bar; };
///     in
///         { inherit foo baz; }
/// "#;
///
/// let lexer = Lexer::new(expr)?;
/// println!("produced tokens: {:?}", lexer.tokens());
/// println!("lexing errors: {:?}", lexer.errors());
/// # Ok(())
/// # }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Lexer<'a> {
    tokens: Vec<Token<'a>>,
    errors: Errors,
}

impl<'a> Lexer<'a> {
    /// Constructs a new `Lexer` and converts the source text into a sequence of tokens.
    ///
    /// This constructor strips all [`Token::Whitespace`] tokens out of the text.
    ///
    /// [`Token::Whitespace`]: ./enum.Token.html#variant.Whitespace
    ///
    /// Once the text has been tokenized, all tokens of type [`Token::Unknown`] will be stripped
    /// out and converted to errors and delimiter balancing is checked. Errors are produced during
    /// these two phases are not fatal, though, and are accessible via the [`errors`] method.
    ///
    /// [`Token::Unknown`]: ./enum.Token.html#variant.Unknown
    /// [`errors`]: #method.errors
    ///
    /// The tokenization process will only return `Err` if the source text is empty or consists
    /// only of comments or invalid tokens, or if the lexer itself has a bug.
    pub fn new(s: &'a str) -> Result<Self, Errors> {
        Self::new_filtered(s, |token| !token.is_whitespace())
    }

    /// Constructs a new `Lexer` and converts the source text into a sequence of tokens.
    ///
    /// This constructor preserves all [`Token::Whitespace`] tokens.
    ///
    /// [`Token::Whitespace`]: ./enum.Token.html#variant.Whitespace
    ///
    /// Once the text has been tokenized, all tokens of type [`Token::Unknown`] will be stripped
    /// out and converted to errors and delimiter balancing is checked. Errors are produced during
    /// these two phases are not fatal, though, and are accessible via the [`errors`] method.
    ///
    /// [`Token::Unknown`]: ./enum.Token.html#variant.Unknown
    /// [`errors`]: #method.errors
    ///
    /// The tokenization process will only return `Err` if the source text is empty or consists
    /// only of comments or invalid tokens, or if the lexer itself has a bug.
    pub fn new_with_whitespace(s: &'a str) -> Result<Self, Errors> {
        Self::new_filtered(s, |_| true)
    }

    fn new_filtered<F>(s: &'a str, f: F) -> Result<Self, Errors>
    where
        F: Fn(&Token) -> bool,
    {
        let input = LocatedSpan::new(s);
        let mut lexer = iterator(input, token);
        let (mut tokens, mut errors) = filter_unexpected_tokens(lexer.filter(f));

        match lexer.finish() {
            Ok((remaining, _)) if remaining.fragment.is_empty() => {
                let end = input.fragment.len().saturating_sub(1) as u32;
                let eof_span = Span::new(end, end);

                let only_trivia = tokens.iter().all(|t| t.is_trivia());
                let errors = if tokens.is_empty() || only_trivia {
                    let message = "Nix expressions must resolve to a value".into();
                    errors.push(Error::Message(Span::initial(), message));
                    return Err(errors);
                } else {
                    errors.extend(check_delims_balanced(&tokens, eof_span));
                    errors
                };

                tokens.push(Token::Eof(eof_span));
                Ok(Lexer { tokens, errors })
            }
            Ok((remaining, _)) => {
                let mut errors = Errors::new();
                errors.push(Error::Nom(remaining.to_span(), ErrorKind::Eof));
                Err(errors)
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
                errors.push(Error::Message(Span::initial(), message.into()));
                Err(errors)
            }
        }
    }

    /// Returns a slice over the tokens produced by the `Lexer`.
    ///
    /// This slice is guaranteed not to include any tokens that are [`Token::Unknown`], as these
    /// are converted to errors accessible via the [`errors`] method.
    ///
    /// [`Token::Unknown`]: ./enum.Token.html#variant.Unknown
    /// [`errors`]: #method.errors
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::error::Errors;
    /// # use nix_parser::lexer::Lexer;
    /// # fn main() -> Result<(), Errors> {
    /// use codespan::Span;
    /// use nix_parser::lexer::Token;
    ///
    /// let expr = "{ foo = /nix/store/xrnya64zahcibg89vqhf9cwm5gcnaa6q-rust; }";
    /// let tokens = [
    ///     Token::LBrace(Span::new(0, 1)),
    ///     Token::Identifier("foo".into(), Span::new(2, 5)),
    ///     Token::Eq(Span::new(6, 7)),
    ///     Token::Path("/nix/store/xrnya64zahcibg89vqhf9cwm5gcnaa6q-rust".into(), Span::new(8, 56)),
    ///     Token::Semi(Span::new(56, 57)),
    ///     Token::RBrace(Span::new(58, 59)),
    ///     Token::Eof(Span::new(58, 58)),
    /// ];
    ///
    /// let lexer = Lexer::new(expr)?;
    /// assert_eq!(lexer.tokens(), &tokens[..]);
    /// # Ok(())
    /// # }
    /// ```
    pub fn tokens(&self) -> Tokens<'_> {
        Tokens::new(self.tokens.as_slice())
    }

    /// Returns the errors that occurred at tokenization time, if any.
    ///
    /// Examples of errors that can occur may include unbalanced delimiters, incorrect delimiters,
    /// the presence of invalid or unknown tokens, path values with trailing slashes, or
    /// unterminated strings or interpolations.
    ///
    /// # Examples
    ///
    /// ```
    /// # use nix_parser::error::Errors;
    /// # use nix_parser::lexer::Lexer;
    /// # fn main() -> Result<(), Errors> {
    /// use codespan::Span;
    ///
    /// let lexer = Lexer::new("{ % is not allowed")?;
    ///
    /// // Two lexing errors: there is an unclosed `{` delimiter, and `%` is an unknown character.
    /// assert_eq!(lexer.errors().len(), 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn errors(&self) -> &Errors {
        &self.errors
    }
}

fn token(input: LocatedSpan) -> IResult<Token> {
    alt((
        whitespace,
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

fn whitespace(input: LocatedSpan) -> IResult<Token> {
    map(multispace1, |span: LocatedSpan| {
        Token::Whitespace(span.to_span())
    })(input)
}

fn unknown(input: LocatedSpan) -> IResult<Token> {
    map(take(1usize), |span: LocatedSpan| {
        let error = UnexpectedError::new(format!("`{}`", span.fragment), span.to_span());
        Token::Unknown(span.fragment.into(), span.to_span(), error.into())
    })(input)
}

fn filter_unexpected_tokens<'a, I>(tokens: I) -> (Vec<Token<'a>>, Errors)
where
    I: Iterator<Item = Token<'a>>,
{
    let mut valid = Vec::new();
    let mut errors = Errors::new();

    for token in tokens {
        match token {
            Token::Unknown(_, _, error) => errors.push(error),
            token => valid.push(token),
        }
    }

    (valid, errors)
}
