use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace0};
use nom::combinator::{cond, peek, recognize};
use nom::multi::many_till;
use nom::sequence::pair;

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
    trim_indent: bool,
) -> impl Fn(LocatedSpan<'a>) -> IResult<'a, Token> {
    move |input| {
        let (input, _) = pair(tag(delimiter), cond(trim_indent, multispace0))(input)?;

        let mut remaining = input;
        let mut fragments = Vec::new();

        loop {
            if let Ok((input, _)) = tag::<_, _, Errors>(delimiter)(remaining) {
                remaining = input;
                break;
            } else if let Ok((input, _)) = punct_interpolate(remaining) {
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
                        _ => {}
                    }
                    tokens.push(token);
                }

                let span = Span::from(input.to_span().start()..remaining.to_span().end());
                fragments.push(StringFragment::Interpolation(tokens, span));
            } else {
                let boundary = alt((tag(delimiter), recognize(punct_interpolate)));
                let (input, string) = recognize(many_till(anychar, peek(boundary)))(remaining)?;
                remaining = input;

                let (span, string) = if trim_indent {
                    let lines: Vec<_> = split_lines_without_indentation(string).collect();
                    (string.to_span(), lines.join("\n"))
                } else {
                    (string.to_span(), string.fragment.to_string())
                };

                fragments.push(StringFragment::Literal(string, span));
            }
        }

        let span = Span::from(input.to_span().start()..remaining.to_span().end());
        Ok((remaining, Token::String(fragments, span)))
    }
}
