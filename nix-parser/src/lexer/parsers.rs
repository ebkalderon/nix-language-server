pub use self::string::string;

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::{is_a, tag};
use nom::character::complete::{
    alpha1, alphanumeric1, anychar, char, line_ending, multispace0, not_line_ending, space0,
};

use nom::combinator::{cond, map, not, peek, recognize};
use nom::multi::{many0, many1, many_till, separated_nonempty_list};
use nom::sequence::{pair, preceded, terminated, tuple};
use nom::Slice;
use once_cell::sync::OnceCell;
use regex::Regex;

use self::number::{float, integer};
use self::path::{path, path_template};
use super::util::{map_spanned, split_lines_without_indentation};
use super::{token, IResult, LocatedSpan, StringFragment, Token};
use crate::error::{Error, Errors};
use crate::ToSpan;

mod number;
mod path;
mod string;

pub fn comment(input: LocatedSpan) -> IResult<Token> {
    let span = map(not_line_ending, |s: LocatedSpan| s.fragment);
    let rows = separated_nonempty_list(pair(line_ending, space0), preceded(char('#'), span));
    let line_comment = map_spanned(rows, |span, r| Token::Comment(r.join("\n"), span.to_span()));
    terminated(alt((line_comment, block_comment)), multispace0)(input)
}

fn block_comment(input: LocatedSpan) -> IResult<Token> {
    let close_tag = recognize(pair(many1(char('*')), char('/')));
    let maybe_consume_stars = alt((peek(close_tag), recognize(many0(char('*')))));
    let (input, _) = tuple((tag("/*"), maybe_consume_stars, multispace0))(input)?;

    static REGEX: OnceCell<Regex> = OnceCell::new();
    let regex = REGEX.get_or_init(|| Regex::new(r#"\*+/"#).unwrap());

    if let Some(m) = regex.find(input.fragment) {
        let span = input.slice(..m.start());
        let rows: Vec<_> = split_lines_without_indentation(span).collect();
        let remaining = input.slice(m.end()..);
        Ok((remaining, Token::Comment(rows.join("\n"), span.to_span())))
    } else {
        let end = input.fragment.len();
        let remaining = input.slice((end - 1)..end);
        let error = Error::Message(input.to_span(), "unterminated block comment".to_string());
        let unknown = Token::Unknown(input.fragment.into(), remaining.to_span(), error);
        Ok((remaining, unknown))
    }
}

pub fn identifier(input: LocatedSpan) -> IResult<Token> {
    let first = alt((alpha1, is_a("_")));
    let rest = alt((alphanumeric1, is_a("_-'")));
    let ident = terminated(recognize(pair(first, many0(rest))), multispace0);
    map_spanned(ident, |span, ident| {
        Token::Identifier(ident.fragment.into(), span)
    })(input)
}

pub fn literal(input: LocatedSpan) -> IResult<Token> {
    alt((boolean, null, path, path_template, float, integer))(input)
}

fn boolean(input: LocatedSpan) -> IResult<Token> {
    let true_val = map_spanned(tag("true"), |span, _| Token::Boolean(true, span));
    let false_val = map_spanned(tag("false"), |span, _| Token::Boolean(false, span));
    alt((true_val, false_val))(input)
}

fn null(input: LocatedSpan) -> IResult<Token> {
    map_spanned(tag("null"), |span, _| Token::Null(span))(input)
}

pub fn interpolation(input: LocatedSpan) -> IResult<Token> {
    let (mut remaining, _) = punct_interpolate(input)?;

    let mut tokens = Vec::new();
    let mut depth = 1;
    loop {
        if let Ok((input, token)) = token(remaining) {
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
            let remaining = input.slice((end - 1)..end);
            let error = Error::Message(input.to_span(), "unterminated interpolation".to_string());
            let unknown = Token::Unknown(input.fragment.into(), remaining.to_span(), error);
            return Ok((remaining, unknown));
        }
    }

    let span = Span::from(input.to_span().start()..remaining.to_span().end());
    Ok((remaining, Token::Interpolation(tokens, span)))
}

macro_rules! define_keywords {
    ($($function:ident => $variant:ident ( $keyword:expr )),+) => {
        pub fn keyword(input: LocatedSpan) -> IResult<Token> {
            let terms = alt(($($function),+));
            terminated(terms, multispace0)(input)
        }

        $(
            pub fn $function(input: LocatedSpan) -> IResult<Token> {
                let keyword = map(tag($keyword), |s: LocatedSpan| Token::$variant(s.to_span()));
                terminated(keyword, peek(not(alpha1)))(input)
            }
        )+
    };
}

define_keywords! {
    keyword_assert => Assert("assert"),
    keyword_else => Else("else"),
    keyword_if => If("if"),
    keyword_in => In("in"),
    keyword_inherit => Inherit("inherit"),
    keyword_let => Let("let"),
    keyword_or => Or("or"),
    keyword_rec => Rec("rec"),
    keyword_then => Then("then"),
    keyword_with => With("with")
}

macro_rules! define_operator {
    ($($function:ident => $variant:ident ( $op:expr )),+) => {
        pub fn operator(input: LocatedSpan) -> IResult<Token> {
            alt(($($function),+))(input)
        }

        $(
            fn $function(input: LocatedSpan) -> IResult<Token> {
                let op = map(tag($op), |span: LocatedSpan| Token::$variant(span.to_span()));
                terminated(op, multispace0)(input)
            }
        )+
    };
}

define_operator! {
    op_add => Add("+"),
    op_sub => Sub("-"),
    op_mul => Mul("*"),
    op_div => Div("/"),
    op_eq => IsEq("=="),
    op_neq => NotEq("!="),
    op_lt => LessThan("<"),
    op_lte => LessThanEq("<="),
    op_gt => GreaterThan(">"),
    op_gte => GreaterThanEq(">="),
    op_and => LogicalAnd("&&"),
    op_or => LogicalOr("||"),
    op_concat => Concat("++"),
    op_update => Update("//"),
    op_has_attr => Question("?"),
    op_imply => Imply("->"),
    op_not => Not("!")
}

macro_rules! define_punctuation {
    ($($function:ident => $variant:ident ( $punct:expr )),+) => {
        pub fn punctuation(input: LocatedSpan) -> IResult<Token> {
            terminated(alt(($($function),+)), multispace0)(input)
        }

        $(
            pub fn $function(input: LocatedSpan) -> IResult<Token> {
                map(tag($punct), |s: LocatedSpan| Token::$variant(s.to_span()))(input)
            }
        )+
    };
}

define_punctuation! {
    punct_colon => Colon(":"),
    punct_comma => Comma(","),
    punct_dot => Dot("."),
    punct_eq => Eq("="),
    punct_interpolate => Interpolate("${"),
    punct_left_brace => LBrace("{"),
    punct_right_brace => RBrace("}"),
    punct_left_bracket => LBracket("["),
    punct_right_bracket => RBracket("]"),
    punct_left_paren => LParen("("),
    punct_right_paren => RParen(")"),
    punct_quote_double => QuoteDouble("\""),
    punct_quote_single => QuoteSingle("''"),
    punct_quote_semi => Semi(";")
}
