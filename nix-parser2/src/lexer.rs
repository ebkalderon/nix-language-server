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
use nom::multi::{many0_count, many1_count, many_till};
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
#[derive(Clone, Copy)]
enum Mode {
    /// Default lexer mode.
    Normal,
    /// Mode used when tokenizing a string.
    String(StringKind),
}

struct LexerModes(SmallVec<[Mode; 8]>);

impl LexerModes {
    fn new() -> Self {
        LexerModes(SmallVec::with_capacity(8))
    }

    fn current(&self) -> &Mode {
        self.0.last().unwrap_or(&Mode::Normal)
    }

    fn push(&mut self, mode: Mode) {
        self.0.push(mode)
    }

    fn pop(&mut self) {
        self.0.pop();
    }
}

/// Converts an input string into a sequence of tokens.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    use TokenKind::*;

    let mut modes = LexerModes::new();
    let mut input = LocatedSpan::new(input);
    std::iter::from_fn(move || {
        let (remaining, out) = next_token(input, *modes.current())?;
        input = remaining;

        match (modes.current(), out.kind) {
            // Switch to and from `Normal` and `String` modes when encountering `"` or `''`.
            (Mode::Normal, StringTerm { kind }) => modes.push(Mode::String(kind)),
            (Mode::String(kind), StringTerm { kind: term_kind }) => {
                debug_assert_eq!(*kind, term_kind);
                modes.pop();
            }

            // Switch back to `Normal` mode when a string interpolation is detected.
            (Mode::String(_), Interpolate) => modes.push(Mode::Normal),

            // Count opening and closing braces, popping back to the previous mode, if any.
            (Mode::Normal, Interpolate) => modes.push(Mode::Normal),
            (Mode::Normal, OpenBrace) => modes.push(Mode::Normal),
            (Mode::Normal, CloseBrace) => modes.pop(),

            _ => (),
        }

        Some(out)
    })
}

fn next_token(input: LocatedSpan, mode: Mode) -> Option<(LocatedSpan, Token)> {
    let result = match mode {
        Mode::Normal => alt((
            line_comment,
            block_comment,
            whitespace,
            path,
            uri,
            identifier,
            float,
            integer,
            path_template,
            interpolate,
            symbol,
            string_term,
            unknown,
        ))(input),
        Mode::String(_) => alt((interpolate, string_literal(mode), string_term))(input),
    };

    match result {
        Ok((remaining, token)) => Some((remaining, token)),
        Err(nom::Err::Error(_)) => None,
        Err(nom::Err::Failure(err)) => unreachable!("lexer failed with: {:?}", err),
        Err(nom::Err::Incomplete(inc)) => unreachable!("lexer returned incomplete: {:?}", inc),
    }
}

fn line_comment(input: LocatedSpan) -> IResult<Token> {
    let comment = recognize(preceded(char('#'), not_line_ending));
    map(comment, |span: LocatedSpan| {
        Token::new(TokenKind::LineComment, span.to_span())
    })(input)
}

fn block_comment(input: LocatedSpan) -> IResult<Token> {
    let (remaining, span, terminated) = match recognize(pair(tag("/*"), take_until("*/")))(input) {
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

fn is_path_segment(c: char) -> bool {
    c.is_ascii_alphanumeric() || "._-+".contains(c)
}

fn path(input: LocatedSpan) -> IResult<Token> {
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
    let segment = take_while1(is_path_segment);
    let segments = terminated(&segment, many0_count(preceded(char('/'), &segment)));
    let path = terminated(segments, opt(char('/')));
    let (remaining, span) = recognize(delimited(char('<'), path, char('>')))(input)?;

    let trailing_slash = span.fragment().ends_with('/');
    let token_kind = TokenKind::Literal {
        kind: LiteralKind::PathTemplate { trailing_slash },
    };

    Ok((remaining, Token::new(token_kind, span.to_span())))
}

fn interpolate(input: LocatedSpan) -> IResult<Token> {
    map(tag("${"), |span: LocatedSpan| {
        Token::new(TokenKind::Interpolate, span.to_span())
    })(input)
}

fn symbol(input: LocatedSpan) -> IResult<Token> {
    let punctuation = alt((
        map(tag(";"), |span: LocatedSpan| (TokenKind::Semi, span)),
        map(tag(","), |span| (TokenKind::Comma, span)),
        map(tag("..."), |span| (TokenKind::Ellipsis, span)),
        map(tag("."), |span| (TokenKind::Dot, span)),
        map(tag("{"), |span| (TokenKind::OpenBrace, span)),
        map(tag("}"), |span| (TokenKind::CloseBrace, span)),
        map(tag("("), |span| (TokenKind::OpenParen, span)),
        map(tag(")"), |span| (TokenKind::CloseParen, span)),
        map(tag("["), |span| (TokenKind::OpenBracket, span)),
        map(tag("]"), |span| (TokenKind::CloseBracket, span)),
        map(tag("?"), |span| (TokenKind::Question, span)),
        map(tag(":"), |span| (TokenKind::Colon, span)),
        map(tag("@"), |span| (TokenKind::At, span)),
    ));

    let token = alt((
        punctuation,
        map(tag("=="), |span| (TokenKind::IsEq, span)),
        map(tag("="), |span| (TokenKind::Eq, span)),
        map(tag("++"), |span| (TokenKind::Concat, span)),
        map(tag("+"), |span| (TokenKind::Add, span)),
        map(tag("->"), |span| (TokenKind::Imply, span)),
        map(tag("-"), |span| (TokenKind::Sub, span)),
        map(tag("*"), |span| (TokenKind::Mul, span)),
        map(tag("//"), |span| (TokenKind::Update, span)),
        map(tag("/"), |span| (TokenKind::Div, span)),
        map(tag("!="), |span| (TokenKind::NotEq, span)),
        map(tag("!"), |span| (TokenKind::Not, span)),
        map(tag("<="), |span| (TokenKind::LessThanEq, span)),
        map(tag("<"), |span| (TokenKind::LessThan, span)),
        map(tag(">="), |span| (TokenKind::GreaterThanEq, span)),
        map(tag(">"), |span| (TokenKind::GreaterThan, span)),
        map(tag("&&"), |span| (TokenKind::LogicalAnd, span)),
        map(tag("||"), |span| (TokenKind::LogicalOr, span)),
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

fn string_literal<'a>(mode: Mode) -> impl Fn(LocatedSpan<'a>) -> IResult<Token> {
    fn many_till_ne<'a, F, G>(f: F, term: G) -> impl Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>
    where
        F: Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>,
        G: Fn(LocatedSpan<'a>) -> IResult<LocatedSpan>,
    {
        move |i| {
            if peek(&term)(i).is_ok() {
                return Err(nom::Err::Error((i, ErrorKind::NonEmpty)));
            }

            let literal_chunk = alt((&f, recognize(anychar)));
            match recognize(many_till(literal_chunk, peek(&term)))(i) {
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
            Mode::String(StringKind::Normal) => {
                let escape = alt((tag("\\\""), tag("\\${")));
                let end_tag = tag("\"");
                many_till_ne(escape, alt((end_tag, recognize(interpolate))))(input)?
            }
            Mode::String(StringKind::Indented) => {
                let escape = terminated(tag("''"), one_of("'$"));
                let end_tag = terminated(tag("''"), peek(none_of("'$")));
                many_till_ne(escape, alt((end_tag, recognize(interpolate))))(input)?
            }
            _ => panic!("string_literal() called outside string lexer state"),
        };

        let token_kind = TokenKind::StringLiteral;
        Ok((remaining, Token::new(token_kind, span.to_span())))
    }
}
