use codespan::Span;
use nom::Slice;

use super::{IResult, LocatedSpan, Token};
use crate::error::{Error, Errors, IncorrectDelimError};
use crate::ToSpan;

/// Combinator which behaves like `nom::combinator::map()`, except it also includes a `ByteSpan`
/// based on the consumed input.
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

#[derive(Debug)]
enum Delimiter {
    LBrace(Span),
    LBracket(Span),
    LParen(Span),
}

pub fn check_delims_balanced(tokens: &[Token]) -> Errors {
    let mut delim_stack = Vec::new();
    let mut errors = Errors::new();

    for token in tokens.iter() {
        println!("stack: {:?}", delim_stack);
        match token {
            Token::LBrace(span) => delim_stack.push(Delimiter::LBrace(*span)),
            Token::LBracket(span) => delim_stack.push(Delimiter::LBracket(*span)),
            Token::LParen(span) => delim_stack.push(Delimiter::LParen(*span)),
            Token::RBrace(span) => match delim_stack.pop() {
                None | Some(Delimiter::LBrace(_)) => {}
                Some(Delimiter::LBracket(unclosed)) | Some(Delimiter::LParen(unclosed)) => {
                    errors.push(IncorrectDelimError::new('}', *span, None, Some(unclosed)));
                }
            },
            Token::RBracket(span) => match delim_stack.pop() {
                None | Some(Delimiter::LBracket(_)) => {}
                Some(Delimiter::LBrace(unclosed)) | Some(Delimiter::LParen(unclosed)) => {
                    errors.push(IncorrectDelimError::new(']', *span, None, Some(unclosed)));
                }
            },
            Token::RParen(span) => match delim_stack.pop() {
                None | Some(Delimiter::LParen(_)) => {}
                Some(Delimiter::LBrace(unclosed)) | Some(Delimiter::LBracket(unclosed)) => {
                    errors.push(IncorrectDelimError::new(')', *span, None, Some(unclosed)));
                }
            },
            _ => continue,
        }
    }

    if !delim_stack.is_empty() {
        let mut unmatched = Vec::with_capacity(delim_stack.len());
        while let Some(delim) = delim_stack.pop() {
            match delim {
                Delimiter::LBrace(span) => unmatched.push(('{', span)),
                Delimiter::LBracket(span) => unmatched.push(('[', span)),
                Delimiter::LParen(span) => unmatched.push(('(', span)),
            }
        }
        // FIXME: Push `un-closed delimiter` error.
    }

    errors
}

pub fn split_lines_without_indentation(input: LocatedSpan) -> impl Iterator<Item = &str> {
    let indent_level = input.get_column();
    input.fragment.lines().map(move |row| {
        let trim_start = row
            .char_indices()
            .take_while(|(i, c)| c.is_whitespace() && *i < indent_level)
            .count();
        &row[trim_start..]
    })
}
