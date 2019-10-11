use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, is_not, tag};
use nom::character::complete::{anychar, char, multispace0, one_of};
use nom::combinator::{cond, map, peek, recognize};
use nom::multi::many_till;
use nom::sequence::{pair, terminated};

use super::{punct_interpolate, punct_quote_double, punct_quote_single};
use crate::lexer::util::split_lines_without_indentation;
use crate::lexer::{token, IResult, LocatedSpan, StringFragment, Token};
use crate::ToSpan;

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
                let start = remaining;
                remaining = input;

                let mut tokens = Vec::new();
                let mut depth = 1;
                while let Ok((input, token)) = token(remaining) {
                    remaining = input;
                    match token {
                        Token::LBrace(_) | Token::Interpolate(_) => depth += 1,
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

                let span = Span::new(start.offset as u32, remaining.offset as u32);
                fragments.push(StringFragment::Interpolation(tokens, span));
            } else {
                let boundary = alt((&delimiter, punct_interpolate));
                let (string, span) = if is_multiline {
                    let (input, string) = recognize(many_till(anychar, peek(boundary)))(remaining)?;
                    let lines: Vec<_> = split_lines_without_indentation(string).collect();
                    remaining = input;
                    (lines.join("\n"), string.to_span())
                } else {
                    let escape = recognize(pair(tag("\\"), one_of("\\\"$")));
                    let chars = alt((escape, recognize(anychar)));
                    let (input, string) = recognize(many_till(chars, peek(boundary)))(remaining)?;
                    let (_, escaped) = escaped_transform(is_not("\\"), '\\', escape_codes)(string)?;
                    remaining = input;
                    (escaped, string.to_span())
                };

                fragments.push(StringFragment::Literal(string, span));
            }
        }

        let span = Span::new(start.offset as u32, remaining.offset as u32);
        Ok((remaining, Token::String(fragments, span)))
    }
}

fn escape_codes(input: LocatedSpan) -> IResult<char> {
    alt((
        map(char('n'), |_| '\n'),
        map(char('r'), |_| '\r'),
        map(char('t'), |_| '\t'),
        anychar,
    ))(input)
}
