//! Combinators for lexing single and multi-line strings.

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::{escaped_transform, is_not, tag};
use nom::character::complete::{anychar, char, multispace0, one_of};
use nom::combinator::{cond, map, not, peek, recognize};
use nom::multi::many_till;
use nom::sequence::{pair, terminated};

use super::punct_interpolate;
use crate::lexer::util::split_lines_without_indent;
use crate::lexer::{token, IResult, LocatedSpan, StringFragment, Token};
use crate::ToSpan;

pub fn string(input: LocatedSpan) -> IResult<Token> {
    let single = string_body(tag("\""), false);
    let multi = string_body(tag("''"), true);
    alt((single, multi))(input)
}

fn string_body<'a, F>(
    delimiter: F,
    is_multiline: bool,
) -> impl Fn(LocatedSpan<'a>) -> IResult<'a, Token>
where
    F: Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>,
{
    move |input| {
        let start = input;
        let (input, _) = pair(&delimiter, cond(is_multiline, multispace0))(input)?;

        let mut remaining = input;
        let mut fragments = Vec::new();
        let indent_level = input.get_column();

        loop {
            if let Ok((input, _)) = pair(&delimiter, peek(not(char('$'))))(remaining) {
                remaining = input;
                break;
            } else if let Ok((input, _)) = punct_interpolate(remaining) {
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
                        _ => {}
                    }
                    tokens.push(token);
                }

                let span = Span::new(start.offset as u32, remaining.offset as u32);
                fragments.push(StringFragment::Interpolation(tokens, span));
            } else {
                let (string, span) = if is_multiline {
                    let unescaped_delim = terminated(&delimiter, peek(not(char('$'))));
                    let boundary = alt((unescaped_delim, recognize(punct_interpolate)));
                    let chars = alt((tag("''$"), recognize(anychar)));
                    let (input, string) = recognize(many_till(chars, peek(boundary)))(remaining)?;
                    let lines: Vec<_> = split_lines_without_indent(string, indent_level).collect();
                    remaining = input;
                    (lines.join("\n").replace("''$", "$"), string.to_span())
                } else {
                    let unescaped_delim = recognize(terminated(not(char('\\')), peek(&delimiter)));
                    let boundary = alt((unescaped_delim, peek(recognize(punct_interpolate))));
                    let escape = recognize(pair(char('\\'), one_of("\\\"$")));
                    let chars = alt((escape, recognize(anychar)));
                    let (input, string) = recognize(many_till(chars, boundary))(remaining)?;
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
