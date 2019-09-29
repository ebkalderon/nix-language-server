use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace0, one_of};
use nom::combinator::{cond, peek, recognize};
use nom::multi::many_till;
use nom::sequence::{pair, terminated};

use super::punct_interpolate;
use crate::error::Errors;
use crate::lexer::util::split_lines_without_indentation;
use crate::lexer::{token, IResult, LocatedSpan, StringFragment, Token};
use crate::ToSpan;

pub fn string(input: LocatedSpan) -> IResult<Token> {
    let single = string_body("\"", false);
    let multi = string_body("''", true);
    alt((single, multi))(input)
}

fn string_body<'a>(
    delimiter: &'a str,
    is_multiline: bool,
) -> impl Fn(LocatedSpan<'a>) -> IResult<'a, Token> {
    move |input| {
        let start = input;
        let (input, _) = pair(tag(delimiter), cond(is_multiline, multispace0))(input)?;

        let mut remaining = input;
        let mut fragments = Vec::new();

        loop {
            if let Ok((input, _)) = tag::<_, _, Errors>(delimiter)(remaining) {
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
                let boundary = alt((tag(delimiter), recognize(punct_interpolate)));
                let (input, string) = if is_multiline {
                    recognize(many_till(anychar, peek(boundary)))(remaining)?
                } else {
                    let escape = recognize(pair(tag("\\"), one_of("\\$\"")));
                    let skip_int = recognize(pair(tag("\\"), punct_interpolate));
                    let chars = alt((skip_int, escape, recognize(anychar)));
                    recognize(many_till(chars, peek(boundary)))(remaining)?
                };
                remaining = input;

                let (span, string) = if is_multiline {
                    let lines: Vec<_> = split_lines_without_indentation(string).collect();
                    (string.to_span(), lines.join("\n"))
                } else {
                    (string.to_span(), string.fragment.to_string())
                };

                fragments.push(StringFragment::Literal(string, span));
            }
        }

        let span = Span::new(start.to_span().start(), remaining.to_span().start());
        Ok((remaining, Token::String(fragments, span)))
    }
}
