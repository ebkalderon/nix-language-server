//! Types for reading tokens and token slices.

use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use std::slice;

use codespan::Span;
use nom::{InputIter, InputLength, InputTake, Offset, Slice};

use crate::error::Error;
use crate::ToSpan;

/// A slice over a sequence of tokens.
///
/// This struct is produced by the [`tokens`] method on [`Lexer`]. It implements all the necessary
/// traits to be used as an input type with [`nom`] parser combinators.
///
/// [`tokens`]: ./struct.Lexer.html#method.tokens
/// [`Lexer`]: ./struct.Lexer.html
/// [`nom`]: https://docs.rs/nom/5.0.1/nom/
#[derive(Clone, Copy, PartialEq)]
pub struct Tokens<'a> {
    tokens: &'a [Token<'a>],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    /// Constructs a new `Tokens` from a slice.
    pub(crate) fn new(tokens: &'a [Token<'a>]) -> Self {
        Tokens {
            tokens,
            start: 0,
            end: tokens.len(),
        }
    }

    /// Returns a reference to the current token.
    ///
    /// # Panics
    ///
    /// This method will panic if `Tokens` contains no tokens.
    #[inline]
    pub fn current(&self) -> &'a Token<'a> {
        &self.tokens[0]
    }
}

impl<'a> Debug for Tokens<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let slice = &self.tokens[self.start..self.end];
        fmt.debug_tuple(stringify!(Tokens)).field(&slice).finish()
    }
}

impl<'a> Display for Tokens<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let slice = &self.tokens[self.start..self.end];
        fmt.debug_list().entries(slice).finish()
    }
}

impl<'a> Offset for Tokens<'a> {
    fn offset(&self, second: &Self) -> usize {
        const TOKEN_LEN: usize = std::mem::size_of::<Token>();
        let first = self.tokens.as_ptr();
        let second = second.tokens.as_ptr();
        (second as usize - first as usize) / TOKEN_LEN
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tokens: &self.tokens[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        let first = Tokens {
            tokens: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tokens: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<slice::Iter<'a, Token<'a>>>;
    type IterElem = slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(|b| predicate(b))
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tokens.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a, 'b: 'a> PartialEq<&'a [Token<'a>]> for Tokens<'b> {
    fn eq(&self, other: &&'a [Token<'a>]) -> bool {
        self.tokens == *other
    }
}

impl<'a, 'b: 'a> PartialEq<&'a mut [Token<'a>]> for Tokens<'b> {
    fn eq(&self, other: &&'a mut [Token<'a>]) -> bool {
        self.tokens == *other
    }
}

impl<'a, 'b: 'a> PartialEq<Vec<Token<'a>>> for Tokens<'b> {
    fn eq(&self, other: &Vec<Token<'a>>) -> bool {
        self.tokens == &other[..]
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tokens: self.tokens.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tokens: self.tokens,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> ToSpan for Tokens<'a> {
    fn to_span(&self) -> Span {
        let start = self
            .tokens
            .first()
            .map(|t| t.to_span().start())
            .unwrap_or_default();
        let end = self
            .tokens
            .last()
            .map(|t| t.to_span().end())
            .unwrap_or_default();
        Span::new(start, end)
    }
}

/// List of valid types of comments.
///
/// This enum indicates whether a [`Token::Comment`] is a line comment or a block comment.
///
/// [`Token::Comment`]: ./enum.Token.html#variant.Comment
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommentKind {
    Line,
    Block,
}

/// A fragment of a string token.
///
/// A [`Token::String`] is composed of a sequence of `StringFragment`s.
///
/// [`Token::String`]: ./enum.Token.html#variant.String
#[derive(Clone, PartialEq)]
pub enum StringFragment<'a> {
    /// A literal string value.
    ///
    /// If this value is part of a single-line string, all escape codes are normalized. If this
    /// value is part of a multi-line string, the text contents are left alone and leading
    /// identation for the block is trimmed.
    Literal(String, Span),
    /// An interpolated expression.
    Interpolation(Vec<Token<'a>>, Span),
}

impl<'a> Debug for StringFragment<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            StringFragment::Literal(ref text, _) => {
                fmt.debug_tuple("Literal").field(&text).finish()
            }
            StringFragment::Interpolation(ref tokens, _) => {
                fmt.debug_tuple("Interpolation").field(&tokens).finish()
            }
        }
    }
}

/// A token with span information.
#[derive(Clone, PartialEq)]
pub enum Token<'a> {
    Eof(Span),
    Unknown(Cow<'a, str>, Span, Error),

    // Literals
    Comment(String, CommentKind, Span),
    Identifier(Cow<'a, str>, Span),
    Null(Span),
    Boolean(bool, Span),
    Float(Cow<'a, str>, Span),
    Integer(Cow<'a, str>, Span),
    Interpolation(Vec<Token<'a>>, Span),
    Path(Cow<'a, str>, Span),
    PathTemplate(Cow<'a, str>, Span),
    String(Vec<StringFragment<'a>>, Span),
    Uri(Cow<'a, str>, Span),

    // Operators
    Add(Span),
    Sub(Span),
    Mul(Span),
    Div(Span),
    IsEq(Span),
    NotEq(Span),
    LessThan(Span),
    LessThanEq(Span),
    GreaterThan(Span),
    GreaterThanEq(Span),
    LogicalAnd(Span),
    LogicalOr(Span),
    Concat(Span),
    Update(Span),
    Question(Span),
    Imply(Span),
    Not(Span),

    // Keywords
    Assert(Span),
    Else(Span),
    If(Span),
    In(Span),
    Inherit(Span),
    Let(Span),
    Or(Span),
    Rec(Span),
    Then(Span),
    With(Span),

    // Punctuation
    At(Span),
    Colon(Span),
    Comma(Span),
    Dot(Span),
    Ellipsis(Span),
    Eq(Span),
    Interpolate(Span),
    LBrace(Span),
    RBrace(Span),
    LBracket(Span),
    RBracket(Span),
    LParen(Span),
    RParen(Span),
    QuoteDouble(Span),
    QuoteSingle(Span),
    Semi(Span),
}

impl<'a> Token<'a> {
    /// Returns `true` if the token is a [`Token::Comment`].
    ///
    /// [`Token::Comment`]: ./enum.Token.html#variant.Comment
    pub fn is_comment(&self) -> bool {
        match *self {
            Token::Comment(..) => true,
            _ => false,
        }
    }

    /// Returns `true` if the token is a reserved keyword.
    pub fn is_keyword(&self) -> bool {
        match *self {
            Token::Assert(_)
            | Token::Else(_)
            | Token::If(_)
            | Token::In(_)
            | Token::Inherit(_)
            | Token::Let(_)
            | Token::Or(_)
            | Token::Rec(_)
            | Token::Then(_)
            | Token::With(_) => true,
            _ => false,
        }
    }

    /// Returns the user-facing description of a token.
    ///
    /// This string is intended to be used in the text of error messages.
    pub fn description(&self) -> String {
        match *self {
            Token::Eof(_) => "<eof>".to_string(),
            Token::Unknown(ref text, _, _) => format!("`{}`", text.escape_debug()),

            Token::Comment(..) => "comment".to_string(),
            Token::Identifier(ref ident, _) => format!("identifier `{}`", ident),
            Token::Null(_) => "null literal".to_string(),
            Token::Boolean(_, _) => "boolean".to_string(),
            Token::Float(_, _) => "float literal".to_string(),
            Token::Integer(_, _) => "integer literal".to_string(),
            Token::Interpolation(_, _) => "interpolation".to_string(),
            Token::Path(_, _) => "path literal".to_string(),
            Token::PathTemplate(_, _) => "path template".to_string(),
            Token::String(_, _) => "string".to_string(),
            Token::Uri(_, _) => "URI".to_string(),

            Token::Add(_) => "operator `+`".to_string(),
            Token::Sub(_) => "operator `-`".to_string(),
            Token::Mul(_) => "operator `*`".to_string(),
            Token::Div(_) => "operator `/`".to_string(),
            Token::IsEq(_) => "operator `==`".to_string(),
            Token::NotEq(_) => "operator `!=`".to_string(),
            Token::LessThan(_) => "operator `<`".to_string(),
            Token::LessThanEq(_) => "operator `<=`".to_string(),
            Token::GreaterThan(_) => "operator `>`".to_string(),
            Token::GreaterThanEq(_) => "operator `>`".to_string(),
            Token::LogicalAnd(_) => "operator `&&`".to_string(),
            Token::LogicalOr(_) => "operator `||`".to_string(),
            Token::Concat(_) => "operator `++`".to_string(),
            Token::Update(_) => "operator `//`".to_string(),
            Token::Question(_) => "operator `?`".to_string(),
            Token::Imply(_) => "operator `->`".to_string(),
            Token::Not(_) => "unary operator `!`".to_string(),

            Token::Assert(_) => "keyword `assert`".to_string(),
            Token::Else(_) => "keyword `else`".to_string(),
            Token::If(_) => "keyword `if`".to_string(),
            Token::In(_) => "keyword `in`".to_string(),
            Token::Inherit(_) => "keyword `inherit`".to_string(),
            Token::Let(_) => "keyword `let`".to_string(),
            Token::Or(_) => "keyword `or`".to_string(),
            Token::Rec(_) => "keyword `rec`".to_string(),
            Token::Then(_) => "keyword `then`".to_string(),
            Token::With(_) => "keyword `with`".to_string(),

            Token::At(_) => "at symbol (`@`)".to_string(),
            Token::Colon(_) => "colon".to_string(),
            Token::Comma(_) => "comma".to_string(),
            Token::Dot(_) => "dot separator".to_string(),
            Token::Ellipsis(_) => "ellipsis (`...`)".to_string(),
            Token::Eq(_) => "equals sign".to_string(),
            Token::Interpolate(_) => "interpolation sign (`${`)".to_string(),
            Token::LBrace(_) => "left brace".to_string(),
            Token::RBrace(_) => "right brace".to_string(),
            Token::LBracket(_) => "left bracket".to_string(),
            Token::RBracket(_) => "right bracket".to_string(),
            Token::LParen(_) => "left parentheses".to_string(),
            Token::RParen(_) => "right parentheses".to_string(),
            Token::QuoteDouble(_) => "double quote".to_string(),
            Token::QuoteSingle(_) => "multiline string open (`''`)".to_string(),
            Token::Semi(_) => "semicolon".to_string(),
        }
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Token::Eof(_) => fmt.write_str("Eof"),
            Token::Unknown(ref text, _, ref errors) => fmt
                .debug_tuple("Unknown")
                .field(&text)
                .field(&errors)
                .finish(),

            Token::Comment(ref text, ..) => fmt.debug_tuple("Comment").field(&text).finish(),
            Token::Identifier(ref text, _) => fmt.debug_tuple("Identifier").field(&text).finish(),
            Token::Null(_) => fmt.write_str("Null"),
            Token::Boolean(ref value, _) => fmt.debug_tuple("Boolean").field(&value).finish(),
            Token::Float(ref value, _) => fmt.debug_tuple("Float").field(&value).finish(),
            Token::Interpolation(ref value, _) => {
                fmt.debug_tuple("Interpolation").field(&value).finish()
            }
            Token::Integer(ref value, _) => fmt.debug_tuple("Integer").field(&value).finish(),
            Token::Path(ref value, _) => fmt.debug_tuple("Path").field(&value).finish(),
            Token::PathTemplate(ref value, _) => {
                fmt.debug_tuple("PathTemplate").field(&value).finish()
            }
            Token::String(ref value, _) => fmt.debug_tuple("String").field(&value).finish(),
            Token::Uri(ref value, _) => fmt.debug_tuple("Uri").field(&value).finish(),

            Token::Add(_) => fmt.write_str("Add"),
            Token::Sub(_) => fmt.write_str("Sub"),
            Token::Mul(_) => fmt.write_str("Mul"),
            Token::Div(_) => fmt.write_str("Div"),
            Token::IsEq(_) => fmt.write_str("IsEq"),
            Token::NotEq(_) => fmt.write_str("NotEq"),
            Token::LessThan(_) => fmt.write_str("LessThan"),
            Token::LessThanEq(_) => fmt.write_str("LessThanEq"),
            Token::GreaterThan(_) => fmt.write_str("GreaterThan"),
            Token::GreaterThanEq(_) => fmt.write_str("GreaterThanEq"),
            Token::LogicalAnd(_) => fmt.write_str("LogicalAnd"),
            Token::LogicalOr(_) => fmt.write_str("LogicalOr"),
            Token::Concat(_) => fmt.write_str("Concat"),
            Token::Update(_) => fmt.write_str("Update"),
            Token::Question(_) => fmt.write_str("Question"),
            Token::Imply(_) => fmt.write_str("Imply"),
            Token::Not(_) => fmt.write_str("Not"),

            Token::Assert(_) => fmt.write_str("Assert"),
            Token::Else(_) => fmt.write_str("Else"),
            Token::If(_) => fmt.write_str("If"),
            Token::In(_) => fmt.write_str("In"),
            Token::Inherit(_) => fmt.write_str("Inherit"),
            Token::Let(_) => fmt.write_str("Let"),
            Token::Or(_) => fmt.write_str("Or"),
            Token::Rec(_) => fmt.write_str("Rec"),
            Token::Then(_) => fmt.write_str("Then"),
            Token::With(_) => fmt.write_str("With"),

            Token::At(_) => fmt.write_str("At"),
            Token::Colon(_) => fmt.write_str("Colon"),
            Token::Comma(_) => fmt.write_str("Comma"),
            Token::Dot(_) => fmt.write_str("Dot"),
            Token::Ellipsis(_) => fmt.write_str("Ellipsis"),
            Token::Eq(_) => fmt.write_str("Eq"),
            Token::Interpolate(_) => fmt.write_str("Interpolate"),
            Token::LBrace(_) => fmt.write_str("LBrace"),
            Token::RBrace(_) => fmt.write_str("RBrace"),
            Token::LBracket(_) => fmt.write_str("LBracket"),
            Token::RBracket(_) => fmt.write_str("RBracket"),
            Token::LParen(_) => fmt.write_str("LParen"),
            Token::RParen(_) => fmt.write_str("RParen"),
            Token::QuoteDouble(_) => fmt.write_str("QuoteDouble"),
            Token::QuoteSingle(_) => fmt.write_str("QuoteSingle"),
            Token::Semi(_) => fmt.write_str("Semi"),
        }
    }
}

impl<'a> ToSpan for Token<'a> {
    fn to_span(&self) -> Span {
        match *self {
            Token::Eof(ref span) => *span,
            Token::Unknown(_, ref span, _) => *span,

            Token::Comment(_, _, ref span) => *span,
            Token::Identifier(_, ref span) => *span,
            Token::Null(ref span) => *span,
            Token::Boolean(_, ref span) => *span,
            Token::Float(_, ref span) => *span,
            Token::Integer(_, ref span) => *span,
            Token::Interpolation(_, ref span) => *span,
            Token::Path(_, ref span) => *span,
            Token::PathTemplate(_, ref span) => *span,
            Token::String(_, ref span) => *span,
            Token::Uri(_, ref span) => *span,

            Token::Add(ref span) => *span,
            Token::Sub(ref span) => *span,
            Token::Mul(ref span) => *span,
            Token::Div(ref span) => *span,
            Token::IsEq(ref span) => *span,
            Token::NotEq(ref span) => *span,
            Token::LessThan(ref span) => *span,
            Token::LessThanEq(ref span) => *span,
            Token::GreaterThan(ref span) => *span,
            Token::GreaterThanEq(ref span) => *span,
            Token::LogicalAnd(ref span) => *span,
            Token::LogicalOr(ref span) => *span,
            Token::Concat(ref span) => *span,
            Token::Update(ref span) => *span,
            Token::Question(ref span) => *span,
            Token::Imply(ref span) => *span,
            Token::Not(ref span) => *span,

            Token::Assert(ref span) => *span,
            Token::Else(ref span) => *span,
            Token::If(ref span) => *span,
            Token::In(ref span) => *span,
            Token::Inherit(ref span) => *span,
            Token::Let(ref span) => *span,
            Token::Or(ref span) => *span,
            Token::Rec(ref span) => *span,
            Token::Then(ref span) => *span,
            Token::With(ref span) => *span,

            Token::At(ref span) => *span,
            Token::Colon(ref span) => *span,
            Token::Comma(ref span) => *span,
            Token::Dot(ref span) => *span,
            Token::Ellipsis(ref span) => *span,
            Token::Eq(ref span) => *span,
            Token::Interpolate(ref span) => *span,
            Token::LBrace(ref span) => *span,
            Token::RBrace(ref span) => *span,
            Token::LBracket(ref span) => *span,
            Token::RBracket(ref span) => *span,
            Token::LParen(ref span) => *span,
            Token::RParen(ref span) => *span,
            Token::QuoteDouble(ref span) => *span,
            Token::QuoteSingle(ref span) => *span,
            Token::Semi(ref span) => *span,
        }
    }
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset() {
        let tokens = [
            Token::LBrace(Span::default()),
            Token::Identifier(Cow::from("foo"), Span::default()),
            Token::RBrace(Span::default()),
            Token::Colon(Span::default()),
            Token::Integer(Cow::from("42"), Span::default()),
            Token::Eof(Span::default()),
        ];

        let a = Tokens::new(&tokens[..]);
        let b = Tokens::new(&tokens[2..]);
        let c = Tokens::new(&tokens[..4]);
        let d = Tokens::new(&tokens[3..5]);

        assert_eq!(a.offset(&b), 2);
        assert_eq!(a.offset(&c), 0);
        assert_eq!(a.offset(&d), 3);
    }

    #[test]
    fn slice() {
        let tokens = [
            Token::Let(Span::default()),
            Token::Identifier(Cow::from("foo"), Span::default()),
            Token::Eq(Span::default()),
            Token::Path(Cow::from("./."), Span::default()),
            Token::Semi(Span::default()),
            Token::In(Span::default()),
            Token::Identifier(Cow::from("foo"), Span::default()),
            Token::Eof(Span::default()),
        ];

        let full = Tokens::new(&tokens[..]);
        let range_bounded = Tokens {
            tokens: &tokens[1..3],
            start: 1,
            end: 3,
        };
        let range_from = Tokens {
            tokens: &tokens[4..],
            start: 4,
            end: 8,
        };
        let range_to = Tokens {
            tokens: &tokens[..2],
            start: 0,
            end: 2,
        };

        assert_eq!(full.slice(..), full);
        assert_eq!(full.slice(1..3), range_bounded);
        assert_eq!(full.slice(4..), range_from);
        assert_eq!(full.slice(..2), range_to);
    }

    #[test]
    fn to_span() {
        let tokens = [
            Token::Integer(Cow::from("12"), Span::new(0, 2)),
            Token::Add(Span::new(3, 4)),
            Token::Integer(Cow::from("3"), Span::new(5, 6)),
            Token::Eof(Span::new(6, 6)),
        ];

        let a = Tokens::new(&tokens[..]);
        assert_eq!(a.current().to_span(), Span::new(0, 2));
        assert_eq!(a.to_span(), Span::new(0, 6));
    }
}
