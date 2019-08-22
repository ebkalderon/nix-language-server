use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{anychar, char, line_ending},
    combinator::{map, peek, recognize},
    error::{VerboseError, VerboseErrorKind},
    multi::many_till,
    sequence::preceded,
    Slice,
};

use crate::{
    ast::{tokens::Literal, BinaryOp, Expr, ExprBinary},
    parser::{expr::expr, partial::Partial, tokens::space, IResult, Span},
    ToByteSpan,
};

const SINGLE_QUOTE_DELIM: &str = "''";
const SINGLE_QUOTE_ESCAPE_QUOTE: &str = "'''";
const SINGLE_QUOTE_ESCAPE_DOLLAR: &str = "''$";
const SINGLE_QUOTE_ESCAPE_CHAR: &str = r#"''\"#;
const INTERPOLATION_START: &str = "${";
const INTERPOLATION_END: char = '}';

fn single_quote_delim(input: Span) -> IResult<()> {
    map(tag(SINGLE_QUOTE_DELIM), |_| ())(input)
}

fn single_quote_escape_quote(input: Span) -> IResult<()> {
    map(tag(SINGLE_QUOTE_ESCAPE_QUOTE), |_| ())(input)
}

fn single_quote_escape_dollar(input: Span) -> IResult<()> {
    map(tag(SINGLE_QUOTE_ESCAPE_DOLLAR), |_| ())(input)
}

fn single_quote_escape_char(input: Span) -> IResult<()> {
    map(tag(SINGLE_QUOTE_ESCAPE_CHAR), |_| ())(input)
}

fn interpolation_start(input: Span) -> IResult<()> {
    map(tag(INTERPOLATION_START), |_| ())(input)
}

fn interpolation_end(input: Span) -> IResult<()> {
    map(char(INTERPOLATION_END), |_| ())(input)
}

/// multi-line string parser
pub fn single_quote_string(input: Span) -> IResult<Partial<Expr>> {
    // single-quote-string
    let original_input = input;
    let (mut input, _) = tag(SINGLE_QUOTE_DELIM)(input)?; // backtrack
                                                          // single-quote-string-continue
    let mut collected_start = input;
    let mut collected = String::new();
    let mut literals = vec![];
    let mut interpolations = vec![];
    let mut lines = vec![];
    let mut no_delimiter = false;
    // combinators
    loop {
        if let Ok((input_, _)) = line_ending::<_, VerboseError<_>>(input) {
            literals.push((collected, collected_start, input_));
            lines.push((literals, interpolations));
            // next line
            collected = String::new();
            collected_start = input_;
            literals = vec![];
            interpolations = vec![];
            input = input_;
        } else if let Ok((input_, _)) = single_quote_escape_quote(input) {
            collected.push_str(SINGLE_QUOTE_DELIM);
            input = input_;
        } else if let Ok((input_, _)) = single_quote_escape_dollar(input) {
            collected.push_str("$");
            input = input_;
        } else if let Ok((input_, _)) = single_quote_escape_char(input) {
            let (input_, next) = anychar(input_)?;
            match next {
                'n' => collected.push('\n'),
                'r' => collected.push('\r'),
                't' => collected.push('\t'),
                '\\' => collected.push('\\'),
                ch => {
                    collected.push('\\');
                    collected.push(ch);
                }
            }
            input = input_;
        } else if let Ok((input_, _)) = interpolation_start(input) {
            literals.push((collected, collected_start, input_));
            let (input_, mut expr) = preceded(space, expr)(input_)?; // soft-bail
            if let Ok((input_, _)) = interpolation_end(input_) {
                input = input_;
            } else {
                expr.extend_errors(VerboseError {
                    errors: vec![(
                        input_,
                        VerboseErrorKind::Context("missing interpolation delimiter `''`"),
                    )],
                });
                input = input_;
            }
            collected = String::new();
            collected_start = input_;
            interpolations.push(expr);
        } else if let Ok((input_, _)) = single_quote_delim(input) {
            literals.push((collected, collected_start, input_));
            lines.push((literals, interpolations));
            // next line
            input = input_;
            break;
        } else if let Err(_) = anychar::<_, VerboseError<_>>(input) {
            no_delimiter = false; // soft-bail

            literals.push((collected, collected_start, input));
            lines.push((literals, interpolations));
            break;
        } else {
            let (input_, collected_chars) = recognize(many_till(
                anychar,
                peek(alt((
                    single_quote_delim,
                    single_quote_escape_dollar,
                    single_quote_escape_char,
                    interpolation_start,
                    map(line_ending, |_| ()),
                ))),
            ))(input)?;
            collected.push_str(collected_chars.fragment);
            input = input_;
        }
    }
    // trim leading empty line
    if lines.len() > 1 {
        let (ref literals, ref interpolations) = &lines[0];
        if literals.len() == 1 && interpolations.len() == 0 && literals[0].0.len() == 0 {
            lines.drain(0..1);
        }
    }
    // trim indentations
    let mut indent = std::usize::MAX;
    for (ref literals, ref interpolations) in &lines {
        let head = literals
            .first()
            .expect("there must be at least one literal");
        if interpolations.len() == 0 && head.0.len() == 0 {
            continue;
        }
        let this_indent = head
            .0
            .chars()
            .position(|c| c != ' ')
            .unwrap_or(head.0.len());
        indent = std::cmp::Ord::min(indent, this_indent);
    }
    if indent < std::usize::MAX {
        for (ref mut literals, _) in lines.as_mut_slice() {
            let head = &mut literals[0].0;
            if head.len() < indent {
                continue;
            }
            let new_head = String::from(&head[indent..]);
            *head = new_head;
        }
    }
    // paste together literals at ends of lines
    let (literals, interpolations) = {
        let mut itr = lines.into_iter();
        let first = itr.next().expect("there is at least one line");
        itr.fold(
            first,
            |(mut total_literals, mut total_interpolations), (mut literals, mut interpolations)| {
                let last_line_trailing_literal = total_literals
                    .last_mut()
                    .expect("there is at least one literal");
                let this_line_leading_literal = literals
                    .drain(0..1)
                    .next()
                    .expect("there is at least one literal");
                last_line_trailing_literal
                    .0
                    .push_str(&format!("\n{}", this_line_leading_literal.0));
                last_line_trailing_literal.2 = this_line_leading_literal.2;
                total_literals.append(&mut literals);
                total_interpolations.append(&mut interpolations);
                (total_literals, total_interpolations)
            },
        )
    };
    let interpolations: Partial<_> = interpolations.into_iter().collect();
    let mut expr: Partial<_> = {
        let mut literals = literals.into_iter().map(|(s, start, end)| {
            Expr::Literal(Literal::String(
                s,
                original_input
                    .slice(start.offset..end.offset)
                    .to_byte_span(),
            ))
        });
        let first: Partial<_> = literals
            .next()
            .expect("there is at least one literal on each line")
            .into();
        interpolations.flat_map(move |interpolations| {
            interpolations.into_iter().zip(literals).fold(
                first,
                |first, (interpolation, literal)| {
                    first.map(|first| {
                        let first = Expr::Binary(ExprBinary::new(
                            BinaryOp::Add,
                            Box::new(first),
                            Box::new(interpolation),
                            Default::default(),
                        ));
                        Expr::Binary(ExprBinary::new(
                            BinaryOp::Add,
                            Box::new(first),
                            Box::new(literal),
                            Default::default(),
                        ))
                    })
                },
            )
        })
    };
    if no_delimiter {
        expr.extend_errors(VerboseError {
            errors: vec![(
                input,
                VerboseErrorKind::Context("unexpected EOF, expecting end of multiline string"),
            )],
        });
    }
    Ok((input, expr))
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::combinator::all_consuming;

    use crate::ast::{tokens::Ident, Bind, BindSimple, ExprSet};
    #[test]
    fn it_works() {
        let input = Span::new(
            r#"''
hello
world
''"#,
        );
        let (_, val) = all_consuming(single_quote_string)(input).unwrap();
        let span = Span::new("").to_byte_span();
        assert_eq!(
            val,
            Partial::from(Expr::Literal(Literal::String(
                "hello\nworld\n".into(),
                span,
            )))
        );

        let input = Span::new(
            r#"''
  hello
  world
  ${ { a = 10; } }
''"#,
        );
        let (_, val) = all_consuming(single_quote_string)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::Binary(ExprBinary::new(
                BinaryOp::Add,
                Box::new(Expr::Binary(ExprBinary::new(
                    BinaryOp::Add,
                    Box::new(Expr::Literal(Literal::String(
                        "hello\nworld\n".into(),
                        span
                    ))),
                    Box::new(Expr::Set(ExprSet::new(
                        vec![Bind::Simple(BindSimple::new(
                            None,
                            vec![Ident::from("a")].into(),
                            Box::new(Expr::Literal(Literal::Integer(10, span))),
                            span
                        ))],
                        span
                    ))),
                    span,
                ))),
                Box::new(Expr::Literal(Literal::String("\n".into(), span))),
                span
            )))
        );
    }
}
