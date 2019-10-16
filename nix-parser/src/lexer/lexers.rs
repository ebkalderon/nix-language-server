//! Combinators for splitting a source text into tokens.

pub use self::string::string;

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{anychar, char, line_ending, multispace0, not_line_ending, space0};
use nom::combinator::{map, peek, recognize, verify};
use nom::multi::{many0, many1, separated_nonempty_list};
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use self::number::{float, integer};
use self::path::{path, path_template};
use self::uri::uri;
use super::util::{map_spanned, split_lines_without_indentation};
use super::{token, CommentKind, IResult, LocatedSpan, Token};
use crate::error::Error;
use crate::ToSpan;

mod number;
mod path;
mod string;
mod uri;

pub fn comment(input: LocatedSpan) -> IResult<Token> {
    let span = map(not_line_ending, |s: LocatedSpan| s.fragment);
    let rows = separated_nonempty_list(pair(line_ending, space0), preceded(char('#'), span));
    let text = map(rows, |rows| rows.join("\n"));
    let line_comment = map_spanned(text, |span, t| Token::Comment(t, CommentKind::Line, span));
    alt((line_comment, block_comment))(input)
}

fn block_comment(input: LocatedSpan) -> IResult<Token> {
    let close_tag = recognize(pair(many1(char('*')), char('/')));
    let maybe_consume_stars = alt((peek(close_tag), recognize(many0(char('*')))));
    let (input, _) = tuple((tag("/*"), maybe_consume_stars, multispace0))(input)?;

    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| Regex::new(r#"\*+/"#).unwrap());

    if let Some(m) = regex.find(input.fragment) {
        let span = input.slice(..m.start());
        let remaining = input.slice(m.end()..);
        let rows: Vec<_> = split_lines_without_indentation(span).collect();
        let comment = Token::Comment(rows.join("\n"), CommentKind::Block, span.to_span());
        Ok((remaining, comment))
    } else {
        let end = input.fragment.len();
        let remaining = input.slice((end - 1)..end);
        let error = Error::Message(input.to_span(), "unterminated block comment".to_string());
        let unknown = Token::Unknown(input.fragment.into(), remaining.to_span(), error);
        Ok((remaining, unknown))
    }
}

pub fn literal(input: LocatedSpan) -> IResult<Token> {
    alt((boolean, path, float, integer, path_template, uri))(input)
}

fn boolean(input: LocatedSpan) -> IResult<Token> {
    alt((
        map(tag("true"), |s: LocatedSpan| {
            Token::Boolean(true, s.to_span())
        }),
        map(tag("false"), |s: LocatedSpan| {
            Token::Boolean(false, s.to_span())
        }),
    ))(input)
}

pub fn interpolation(input: LocatedSpan) -> IResult<Token> {
    let (mut remaining, _) = terminated(punct_interpolate, multispace0)(input)?;

    let mut tokens = Vec::new();
    let mut depth = 1;
    loop {
        if let Ok((input, token)) = terminated(token, multispace0)(remaining) {
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
        } else {
            let end = input.fragment.len();
            let remaining = input.slice(end..);
            let error = Error::Message(input.to_span(), "unterminated interpolation".to_string());
            let unknown = Token::Unknown(input.fragment.into(), input.to_span(), error);
            return Ok((remaining, unknown));
        }
    }

    let span = Span::new(input.offset as u32, remaining.offset as u32);
    Ok((remaining, Token::Interpolation(tokens, span)))
}

macro_rules! define_identifiers {
    ($($variant:ident ( $keyword:expr )),+) => {
        pub fn identifier(input: LocatedSpan) -> IResult<Token> {
            let first = verify(anychar, |c: &char| c.is_alphabetic() || *c == '_');
            let rest = take_while(|c: char| c.is_alphanumeric() || "_-'".contains(c));
            let ident = recognize(pair(first, rest));
            map(ident, |span: LocatedSpan| match span.fragment {
                $($keyword => Token::$variant(span.to_span()),)+
                frag => Token::Identifier(frag.into(), span.to_span()),
            })(input)
        }
    };
}

define_identifiers! {
    Assert("assert"),
    Else("else"),
    If("if"),
    In("in"),
    Inherit("inherit"),
    Let("let"),
    Null("null"),
    Or("or"),
    Rec("rec"),
    Then("then"),
    With("with")
}

macro_rules! define_operator {
    ($($variant:ident ( $op:expr )),+) => {
        pub fn operator(input: LocatedSpan) -> IResult<Token> {
            alt(($(map(tag($op), |s: LocatedSpan| Token::$variant(s.to_span()))),+))(input)
        }
    };
}

define_operator! {
    Concat("++"),
    Add("+"),
    Imply("->"),
    Sub("-"),
    Mul("*"),
    Update("//"),
    Div("/"),
    NotEq("!="),
    LessThanEq("<="),
    LessThan("<"),
    GreaterThanEq(">="),
    GreaterThan(">"),
    LogicalAnd("&&"),
    LogicalOr("||"),
    Question("?"),
    Not("!")
}

macro_rules! define_punctuation {
    ($($function:ident => $variant:ident ( $punct:expr )),+) => {
        pub fn punctuation(input: LocatedSpan) -> IResult<Token> {
            alt(($($function),+))(input)
        }

        $(
            fn $function(input: LocatedSpan) -> IResult<Token> {
                map(tag($punct), |s: LocatedSpan| Token::$variant(s.to_span()))(input)
            }
        )+
    };
}

define_punctuation! {
    punct_quote_semi => Semi(";"),
    punct_comma => Comma(","),
    punct_ellipsis => Ellipsis("..."),
    punct_dot => Dot("."),
    op_eq => IsEq("=="),
    punct_eq => Eq("="),
    punct_interpolate => Interpolate("${"),
    punct_left_brace => LBrace("{"),
    punct_right_brace => RBrace("}"),
    punct_left_bracket => LBracket("["),
    punct_right_bracket => RBracket("]"),
    punct_left_paren => LParen("("),
    punct_right_paren => RParen(")"),
    punct_colon => Colon(":"),
    punct_token => At("@"),
    punct_quote_double => QuoteDouble("\""),
    punct_quote_single => QuoteSingle("''")
}
