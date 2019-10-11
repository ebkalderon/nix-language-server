use codespan::Span;
use nom::Slice;

use super::{IResult, LocatedSpan, Token};
use crate::error::{Errors, IncorrectDelimError, UnclosedDelimError};
use crate::ToSpan;

/// Combinator which behaves like `nom::combinator::map()`, except it also includes a `Span` based
/// on the consumed input.
pub fn map_spanned<'a, O1, O2, P, F>(parser: P, f: F) -> impl Fn(LocatedSpan<'a>) -> IResult<O2>
where
    P: Fn(LocatedSpan<'a>) -> IResult<O1>,
    F: Fn(Span, O1) -> O2,
{
    move |input| {
        let (remainder, value) = parser(input)?;
        let value_len = remainder.offset - input.offset;
        let span = input.slice(..value_len).to_span();
        Ok((remainder, f(span, value)))
    }
}

pub fn check_delims_balanced(tokens: &[Token], eof_span: Span) -> Errors {
    let mut delim_stack = Vec::new();
    let mut errors = Errors::new();

    for token in tokens.iter() {
        match token {
            delim @ Token::Interpolate(_) => delim_stack.push(delim),
            delim @ Token::LBrace(_) => delim_stack.push(delim),
            delim @ Token::LBracket(_) => delim_stack.push(delim),
            delim @ Token::LParen(_) => delim_stack.push(delim),
            Token::RBrace(span) => match delim_stack.pop() {
                None | Some(Token::LBrace(_)) | Some(Token::Interpolate(_)) => {}
                Some(token) => {
                    let unclosed = token.to_span();
                    errors.push(IncorrectDelimError::new('}', *span, None, Some(unclosed)));
                }
            },
            Token::RBracket(span) => match delim_stack.pop() {
                None | Some(Token::LBracket(_)) => {}
                Some(token) => {
                    let unclosed = token.to_span();
                    errors.push(IncorrectDelimError::new(']', *span, None, Some(unclosed)));
                }
            },
            Token::RParen(span) => match delim_stack.pop() {
                None | Some(Token::LParen(_)) => {}
                Some(token) => {
                    let unclosed = token.to_span();
                    errors.push(IncorrectDelimError::new(')', *span, None, Some(unclosed)));
                }
            },
            _ => continue,
        }
    }

    if !delim_stack.is_empty() {
        errors.push(UnclosedDelimError::new(delim_stack, eof_span));
    }

    errors
}

pub fn split_lines_without_indentation(input: LocatedSpan) -> impl Iterator<Item = &str> {
    let indent_level = input.get_column();
    input.fragment.split('\n').map(move |row| {
        let trim_start = row
            .char_indices()
            .take_while(|(i, c)| c.is_whitespace() && *i < indent_level)
            .count();
        &row[trim_start..]
    })
}
