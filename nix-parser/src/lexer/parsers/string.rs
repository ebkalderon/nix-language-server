use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace0, one_of};
use nom::combinator::{cond, peek, recognize};
use nom::multi::many_till;
use nom::sequence::{pair, terminated};
use once_cell::sync::Lazy;
use regex::Regex;

use super::{punct_interpolate, punct_quote_double, punct_quote_single};
use crate::lexer::util::split_lines_without_indentation;
use crate::lexer::{token, IResult, LocatedSpan, StringFragment, Token};
use crate::ToSpan;

static ESCAPE_CODES: Lazy<Regex> = Lazy::new(|| Regex::new(r#"\\(?P<code>[rnt$\\"])"#).unwrap());

pub fn string(input: LocatedSpan) -> IResult<Token> {
    let single = string_body(punct_quote_double, false);
    let multi = string_body(punct_quote_single, true);
    alt((single, multi))(input)
}

fn string_body<'a, F>(
    delimiter: F,
    is_multiline: bool,
) -> impl Fn(LocatedSpan<'a>) -> IResult<'a, Token>
where
    F: Fn(LocatedSpan<'a>) -> IResult<Token>,
{
    move |input| {
        let start = input;
        let (input, _) = pair(&delimiter, cond(is_multiline, multispace0))(input)?;

        let mut remaining = input;
        let mut fragments = Vec::new();

        loop {
            if let Ok((input, _)) = delimiter(remaining) {
                remaining = input;
                break;
            } else if let Ok((input, _)) = terminated(punct_interpolate, multispace0)(remaining) {
                remaining = input;

                let mut tokens = Vec::new();
                let mut depth = 1;
                while let Ok((input, token)) = token(remaining) {
                    remaining = input;
                    match token {
                        Token::LBrace(_) => depth += 1,
                        Token::RBrace(_) => {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {
                            let (input, _) = multispace0(remaining)?;
                            remaining = input;
                        }
                    }
                    tokens.push(token);
                }

                let span = Span::merge(input.to_span(), remaining.to_span());
                fragments.push(StringFragment::Interpolation(tokens, span));
            } else {
                let boundary = alt((&delimiter, punct_interpolate));
                let (input, string) = if is_multiline {
                    recognize(many_till(anychar, peek(boundary)))(remaining)?
                } else {
                    let escape = recognize(pair(tag("\\"), one_of("\\\"$")));
                    let chars = alt((escape, recognize(anychar)));
                    recognize(many_till(chars, peek(boundary)))(remaining)?
                };
                remaining = input;

                let escaped = if is_multiline {
                    let lines: Vec<_> = split_lines_without_indentation(string).collect();
                    lines.join("\n")
                } else {
                    ESCAPE_CODES.replace_all(string.fragment, "$code").into()
                };

                fragments.push(StringFragment::Literal(escaped, string.to_span()));
            }
        }

        let span = Span::new(start.to_span().start(), remaining.to_span().start());
        Ok((remaining, Token::String(fragments, span)))
    }
}
