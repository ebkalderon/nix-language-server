//! Utilities for breaking strings into token streams.

pub use self::tokens::{LiteralKind, StringKind, Token, TokenKind};

use codespan::Span;
use nom::branch::alt;
use nom::bytes::complete::{is_a, tag, take_until, take_while, take_while1};
use nom::character::complete::{
    anychar, char, digit0, digit1, multispace1, none_of, not_line_ending, one_of,
};
use nom::combinator::{map, opt, peek, recognize, verify};
use nom::error::ErrorKind;
use nom::multi::{many1_count, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use smallvec::SmallVec;

use crate::ToSpan;

mod tokens;

type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

impl<'a> ToSpan for LocatedSpan<'a> {
    fn to_span(&self) -> Span {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        Span::new(start as u32, end as u32)
    }
}

/// A list of all possible lexer modes.
#[derive(Clone, Copy, Debug)]
enum LexerMode {
    /// Default lexer mode.
    Normal,
    /// Mode used when tokenizing a string.
    String(StringKind),
}

/// Represents the current lexer state.
struct LexState(SmallVec<[LexerMode; 1]>);

impl LexState {
    /// Constructs a new lexer state stack.
    pub fn new() -> Self {
        LexState(SmallVec::with_capacity(1))
    }

    /// Returns the current lexer mode.
    pub fn current_mode(&self) -> &LexerMode {
        self.0.last().unwrap_or(&LexerMode::Normal)
    }

    /// Pushes a new lexer mode.
    pub fn push(&mut self, mode: LexerMode) {
        self.0.push(mode)
    }

    /// Restores the previous lexer mode.
    pub fn pop(&mut self) {
        self.0.pop();
    }
}

/// Converts an input string into a sequence of tokens.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut state = LexState::new();
    let mut input = LocatedSpan::new(input);
    std::iter::from_fn(move || match token(*state.current_mode())(input.clone()) {
        Ok((remaining, out)) => {
            use StringKind::*;
            use TokenKind::*;

            match (state.current_mode(), out.kind) {
                (LexerMode::Normal, StringTerm { kind }) => state.push(LexerMode::String(kind)),
                (LexerMode::String(Normal), StringTerm { kind: Normal }) => state.pop(),
                (LexerMode::String(Indented), StringTerm { kind: Indented }) => state.pop(),
                (LexerMode::String(_), StringTerm { .. }) => {
                    panic!("lexer returned TokenKind::StringTerm not matching current lexer mode");
                }
                _ => (),
            }

            input = remaining;
            Some(out)
        }
        Err(nom::Err::Error(_)) => None,
        Err(nom::Err::Failure(err)) => unreachable!("lexer failed with: {:?}", err),
        Err(nom::Err::Incomplete(inc)) => unreachable!("lexer returned incomplete: {:?}", inc),
    })
}

fn token<'a>(mode: LexerMode) -> impl Fn(LocatedSpan<'a>) -> IResult<Token> {
    move |input| match mode {
        LexerMode::Normal => alt((
            line_comment,
            block_comment,
            whitespace,
            path,
            uri,
            identifier,
            float,
            integer,
            path_template,
            punctuation,
            operators,
            string_term,
            unknown,
        ))(input),
        LexerMode::String(_) => alt((string_literal(mode), string_term))(input),
    }
}

fn line_comment(input: LocatedSpan) -> IResult<Token> {
    let comment = recognize(preceded(char('#'), not_line_ending));
    map(comment, |span: LocatedSpan| {
        Token::new(TokenKind::LineComment, span.to_span())
    })(input)
}

fn block_comment(input: LocatedSpan) -> IResult<Token> {
    let i = input.clone();
    let (remaining, span, terminated) = match recognize(pair(tag("/*"), take_until("*/")))(i) {
        Ok((remaining, span)) => (remaining, span, true),
        Err(nom::Err::Error((remaining, ErrorKind::TakeUntil))) => (remaining, input, false),
        Err(err) => return Err(err),
    };

    let token = Token::new(TokenKind::BlockComment { terminated }, span.to_span());
    Ok((remaining, token))
}

fn whitespace(input: LocatedSpan) -> IResult<Token> {
    map(multispace1, |span: LocatedSpan| {
        Token::new(TokenKind::Whitespace, span.to_span())
    })(input)
}

fn identifier(input: LocatedSpan) -> IResult<Token> {
    let first = verify(anychar, |c| c.is_ascii_alphabetic() || *c == '_');
    let rest = take_while(|c: char| c.is_ascii_alphanumeric() || "_-'".contains(c));
    let ident = recognize(preceded(first, rest));
    map(ident, |span: LocatedSpan| {
        Token::new(TokenKind::Ident, span.to_span())
    })(input)
}

fn path(input: LocatedSpan) -> IResult<Token> {
    fn is_path_segment(c: char) -> bool {
        c.is_ascii_alphanumeric() || "._-+".contains(c)
    }

    let segments = many1_count(preceded(char('/'), take_while1(is_path_segment)));
    let relative = map(pair(take_while(is_path_segment), &segments), |_| ());
    let home = map(pair(char('~'), &segments), |_| ());
    let (remaining, span) = recognize(pair(alt((relative, home)), opt(char('/'))))(input)?;

    let trailing_slash = span.fragment().ends_with('/');
    let token_kind = TokenKind::Literal {
        kind: LiteralKind::Path { trailing_slash },
    };

    Ok((remaining, Token::new(token_kind, span.to_span())))
}

fn uri(input: LocatedSpan) -> IResult<Token> {
    let first = verify(anychar, |c| c.is_ascii_alphabetic());
    let rest = take_while(|c: char| c.is_ascii_alphanumeric() || "+-.".contains(c));
    let scheme = preceded(first, rest);

    let path = take_while1(|c: char| c.is_ascii_alphanumeric() || "%/?:@&=+$,-_.!~*'".contains(c));
    let uri = recognize(tuple((scheme, char(':'), path)));

    map(uri, |span: LocatedSpan| {
        let kind = LiteralKind::Uri;
        let token_kind = TokenKind::Literal { kind };
        Token::new(token_kind, span.to_span())
    })(input)
}

fn float(input: LocatedSpan) -> IResult<Token> {
    let first = preceded(is_a("123456789"), digit0);
    let positive = map(tuple((first, char('.'), digit0)), |_| ());
    let fraction = map(tuple((opt(char('0')), char('.'), digit1)), |_| ());
    let exp = tuple((one_of("Ee"), opt(one_of("+-")), digit1));
    let float = recognize(preceded(alt((positive, fraction)), opt(exp)));

    map(float, |span: LocatedSpan| {
        let kind = LiteralKind::Float;
        let token_kind = TokenKind::Literal { kind };
        Token::new(token_kind, span.to_span())
    })(input)
}

fn integer(input: LocatedSpan) -> IResult<Token> {
    map(digit1, |span: LocatedSpan| {
        let kind = LiteralKind::Integer;
        let token_kind = TokenKind::Literal { kind };
        Token::new(token_kind, span.to_span())
    })(input)
}

fn path_template(input: LocatedSpan) -> IResult<Token> {
    let name = take_while1(|c: char| c.is_ascii_alphanumeric() || "/._-+".contains(c));
    let template = recognize(delimited(char('<'), name, char('>')));

    map(template, |span: LocatedSpan| {
        let kind = LiteralKind::PathTemplate;
        let token_kind = TokenKind::Literal { kind };
        Token::new(token_kind, span.to_span())
    })(input)
}

fn punctuation(input: LocatedSpan) -> IResult<Token> {
    let token = alt((
        map(tag("@"), |span: LocatedSpan| (TokenKind::At, span)),
        map(tag(":"), |span| (TokenKind::Colon, span)),
        map(tag("..."), |span| (TokenKind::Ellipsis, span)),
        map(tag("."), |span| (TokenKind::Dot, span)),
        map(tag("${"), |span| (TokenKind::Interpolate, span)),
        map(tag("{"), |span| (TokenKind::OpenBrace, span)),
        map(tag("}"), |span| (TokenKind::CloseBrace, span)),
        map(tag("["), |span| (TokenKind::OpenBracket, span)),
        map(tag("]"), |span| (TokenKind::CloseBracket, span)),
        map(tag("("), |span| (TokenKind::OpenParen, span)),
        map(tag(")"), |span| (TokenKind::CloseParen, span)),
        map(tag(";"), |span| (TokenKind::Semi, span)),
    ));

    map(token, |(kind, span)| Token::new(kind, span.to_span()))(input)
}

fn operators(input: LocatedSpan) -> IResult<Token> {
    let token = alt((
        map(tag("++"), |span: LocatedSpan| (TokenKind::Concat, span)),
        map(tag("+"), |span| (TokenKind::Add, span)),
        map(tag("->"), |span| (TokenKind::Sub, span)),
        map(tag("-"), |span| (TokenKind::Sub, span)),
        map(tag("*"), |span| (TokenKind::Mul, span)),
        map(tag("//"), |span| (TokenKind::Update, span)),
        map(tag("/"), |span| (TokenKind::Div, span)),
        map(tag("=="), |span| (TokenKind::IsEq, span)),
        map(tag("="), |span| (TokenKind::Eq, span)),
        map(tag("!="), |span| (TokenKind::NotEq, span)),
        map(tag("!"), |span| (TokenKind::Not, span)),
        map(tag("<="), |span| (TokenKind::LessThanEq, span)),
        map(tag("<"), |span| (TokenKind::LessThan, span)),
        map(tag(">="), |span| (TokenKind::GreaterThanEq, span)),
        map(tag(">"), |span| (TokenKind::GreaterThan, span)),
        map(tag("&&"), |span| (TokenKind::LogicalAnd, span)),
        map(tag("||"), |span| (TokenKind::LogicalOr, span)),
        map(tag("?"), |span| (TokenKind::Question, span)),
    ));

    map(token, |(kind, span)| Token::new(kind, span.to_span()))(input)
}

fn unknown(input: LocatedSpan) -> IResult<Token> {
    map(recognize(anychar), |span: LocatedSpan| {
        Token::new(TokenKind::Unknown, span.to_span())
    })(input)
}

fn string_term(input: LocatedSpan) -> IResult<Token> {
    let normal = map(tag("\""), |span: LocatedSpan| (StringKind::Normal, span));
    let indented = map(tag("''"), |span: LocatedSpan| (StringKind::Indented, span));
    let term = alt((normal, indented));

    map(term, |(kind, span)| {
        Token::new(TokenKind::StringTerm { kind }, span.to_span())
    })(input)
}

/// FIXME: Support interpolations.
fn string_literal<'a>(mode: LexerMode) -> impl Fn(LocatedSpan<'a>) -> IResult<Token> {
    fn many_till_ne<'a, F, G>(f: F, term: G) -> impl Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>
    where
        F: Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>,
        G: Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>,
    {
        move |i| {
            if peek(&term)(i.clone()).is_ok() {
                return Err(nom::Err::Error((i, ErrorKind::NonEmpty)));
            }

            let literal_chunk = alt((&f, recognize(anychar)));
            match recognize(many_till(literal_chunk, peek(&term)))(i.clone()) {
                Ok((remaining, span)) => Ok((remaining, span)),
                Err(nom::Err::Error((remaining, ErrorKind::Eof))) if !i.fragment().is_empty() => {
                    Ok((remaining, i))
                }
                Err(err) => Err(err),
            }
        }
    }

    move |input| {
        let (remaining, span) = match mode {
            LexerMode::String(StringKind::Normal) => {
                let escape = tag("\\\"");
                let end_tag = tag("\"");
                many_till_ne(escape, end_tag)(input)?
            }
            LexerMode::String(StringKind::Indented) => {
                let escape = terminated(tag("''"), one_of("'$"));
                let end_tag = terminated(tag("''"), peek(none_of("'$")));
                many_till_ne(escape, end_tag)(input)?
            }
            _ => panic!("string_literal() called outside string lexer state"),
        };

        let token_kind = TokenKind::StringLiteral;
        Ok((remaining, Token::new(token_kind, span.to_span())))
    }
}
