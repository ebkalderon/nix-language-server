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
            StringFragment::Literal(_, ref span) => {
                fmt.debug_tuple("Literal").field(&span.to_string()).finish()
            }
            StringFragment::Interpolation(ref tokens, ref span) => fmt
                .debug_tuple("Interpolation")
                .field(tokens)
                .field(&span.to_string())
                .finish(),
        }
    }
}

/// List of valid types of string literals.
///
/// This enum indicates whether a [`Token::String`] is a normal string or an indented string.
///
/// [`Token::String`]: ./enum.Token.html#variant.String
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringKind {
    /// A string literal beginning with a double-quote, `"`.
    Normal,
    /// A string literal beginning with two single quotes, `''`.
    Indented,
}

/// A token with span information.
#[derive(Clone, PartialEq)]
pub enum Token<'a> {
    Eof(Span),
    Unknown(Cow<'a, str>, Span, Error),
    Whitespace(Span),

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
    String(Vec<StringFragment<'a>>, StringKind, Span),
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
    Semi(Span),
}

impl<'a> Token<'a> {
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

    /// Returns `true` if the token is either a [`Token::Whitespace`] or [`Token::Comment`].
    ///
    /// [`Token::Whitespace`]: ./enum.Token.html#variant.Whitespace
    /// [`Token::Comment`]: ./enum.Token.html#variant.Comment
    pub fn is_trivia(&self) -> bool {
        match *self {
            Token::Comment(..) | Token::Whitespace(_) => true,
            _ => false,
        }
    }

    /// Returns `true` if the token is either a [`Token::Whitespace`].
    ///
    /// [`Token::Whitespace`]: ./enum.Token.html#variant.Whitespace
    pub fn is_whitespace(&self) -> bool {
        match *self {
            Token::Whitespace(_) => true,
            _ => false,
        }
    }

    /// Returns the user-facing description of a token.
    ///
    /// This string is intended to be used in the text of error messages.
    pub fn description(&self) -> Cow<'static, str> {
        match *self {
            Token::Eof(_) => "<eof>".into(),
            Token::Unknown(ref text, _, _) => format!("`{}`", text.escape_debug()).into(),
            Token::Whitespace(_) => "whitespace".into(),

            Token::Comment(..) => "comment".into(),
            Token::Identifier(ref ident, _) => format!("identifier `{}`", ident).into(),
            Token::Null(_) => "null literal".into(),
            Token::Boolean(_, _) => "boolean".into(),
            Token::Float(_, _) => "float literal".into(),
            Token::Integer(_, _) => "integer literal".into(),
            Token::Interpolation(_, _) => "interpolation".into(),
            Token::Path(_, _) => "path literal".into(),
            Token::PathTemplate(_, _) => "path template".into(),
            Token::String(_, _, _) => "string".into(),
            Token::Uri(_, _) => "URI".into(),

            Token::Add(_) => "operator `+`".into(),
            Token::Sub(_) => "operator `-`".into(),
            Token::Mul(_) => "operator `*`".into(),
            Token::Div(_) => "operator `/`".into(),
            Token::IsEq(_) => "operator `==`".into(),
            Token::NotEq(_) => "operator `!=`".into(),
            Token::LessThan(_) => "operator `<`".into(),
            Token::LessThanEq(_) => "operator `<=`".into(),
            Token::GreaterThan(_) => "operator `>`".into(),
            Token::GreaterThanEq(_) => "operator `>`".into(),
            Token::LogicalAnd(_) => "operator `&&`".into(),
            Token::LogicalOr(_) => "operator `||`".into(),
            Token::Concat(_) => "operator `++`".into(),
            Token::Update(_) => "operator `//`".into(),
            Token::Question(_) => "operator `?`".into(),
            Token::Imply(_) => "operator `->`".into(),
            Token::Not(_) => "unary operator `!`".into(),

            Token::Assert(_) => "keyword `assert`".into(),
            Token::Else(_) => "keyword `else`".into(),
            Token::If(_) => "keyword `if`".into(),
            Token::In(_) => "keyword `in`".into(),
            Token::Inherit(_) => "keyword `inherit`".into(),
            Token::Let(_) => "keyword `let`".into(),
            Token::Or(_) => "keyword `or`".into(),
            Token::Rec(_) => "keyword `rec`".into(),
            Token::Then(_) => "keyword `then`".into(),
            Token::With(_) => "keyword `with`".into(),

            Token::At(_) => "at symbol (`@`)".into(),
            Token::Colon(_) => "colon".into(),
            Token::Comma(_) => "comma".into(),
            Token::Dot(_) => "dot separator".into(),
            Token::Ellipsis(_) => "ellipsis (`...`)".into(),
            Token::Eq(_) => "equals sign".into(),
            Token::Interpolate(_) => "interpolation sign (`${`)".into(),
            Token::LBrace(_) => "left brace".into(),
            Token::RBrace(_) => "right brace".into(),
            Token::LBracket(_) => "left bracket".into(),
            Token::RBracket(_) => "right bracket".into(),
            Token::LParen(_) => "left parentheses".into(),
            Token::RParen(_) => "right parentheses".into(),
            Token::Semi(_) => "semicolon".into(),
        }
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Token::Eof(ref span) => fmt.debug_tuple("Eof").field(&span.to_string()).finish(),
            Token::Unknown(_, ref span, _) => {
                fmt.debug_tuple("Unknown").field(&span.to_string()).finish()
            }
            Token::Whitespace(ref span) => fmt
                .debug_tuple("Whitespace")
                .field(&span.to_string())
                .finish(),

            Token::Comment(_, ref kind, ref span) => fmt
                .debug_tuple("Comment")
                .field(kind)
                .field(&span.to_string())
                .finish(),
            Token::Identifier(ref text, ref span) => fmt
                .debug_tuple("Identifier")
                .field(text)
                .field(&span.to_string())
                .finish(),
            Token::Null(ref span) => fmt.debug_tuple("Null").field(&span.to_string()).finish(),
            Token::Boolean(_, ref span) => {
                fmt.debug_tuple("Boolean").field(&span.to_string()).finish()
            }
            Token::Float(_, ref span) => fmt.debug_tuple("Float").field(&span.to_string()).finish(),
            Token::Interpolation(ref value, ref span) => fmt
                .debug_tuple("Interpolation")
                .field(value)
                .field(&span.to_string())
                .finish(),
            Token::Integer(_, ref span) => {
                fmt.debug_tuple("Integer").field(&span.to_string()).finish()
            }
            Token::Path(_, ref span) => fmt.debug_tuple("Path").field(&span.to_string()).finish(),
            Token::PathTemplate(_, ref span) => fmt
                .debug_tuple("PathTemplate")
                .field(&span.to_string())
                .finish(),
            Token::String(ref value, ref kind, ref span) => fmt
                .debug_tuple("String")
                .field(value)
                .field(kind)
                .field(&span.to_string())
                .finish(),
            Token::Uri(_, ref span) => fmt.debug_tuple("Uri").field(&span.to_string()).finish(),

            Token::Add(ref span) => fmt.debug_tuple("Add").field(&span.to_string()).finish(),
            Token::Sub(ref span) => fmt.debug_tuple("Sub").field(&span.to_string()).finish(),
            Token::Mul(ref span) => fmt.debug_tuple("Mul").field(&span.to_string()).finish(),
            Token::Div(ref span) => fmt.debug_tuple("Div").field(&span.to_string()).finish(),
            Token::IsEq(ref span) => fmt.debug_tuple("IsEq").field(&span.to_string()).finish(),
            Token::NotEq(ref span) => fmt.debug_tuple("NotEq").field(&span.to_string()).finish(),
            Token::LessThan(ref span) => fmt
                .debug_tuple("LessThan")
                .field(&span.to_string())
                .finish(),
            Token::LessThanEq(ref span) => fmt
                .debug_tuple("LessThanEq")
                .field(&span.to_string())
                .finish(),
            Token::GreaterThan(ref span) => fmt
                .debug_tuple("GreaterThan")
                .field(&span.to_string())
                .finish(),
            Token::GreaterThanEq(ref span) => fmt
                .debug_tuple("GreaterThanEq")
                .field(&span.to_string())
                .finish(),
            Token::LogicalAnd(ref span) => fmt
                .debug_tuple("LogicalAnd")
                .field(&span.to_string())
                .finish(),
            Token::LogicalOr(ref span) => fmt
                .debug_tuple("LogicalOr")
                .field(&span.to_string())
                .finish(),
            Token::Concat(ref span) => fmt.debug_tuple("Concat").field(&span.to_string()).finish(),
            Token::Update(ref span) => fmt.debug_tuple("Update").field(&span.to_string()).finish(),
            Token::Question(ref span) => fmt
                .debug_tuple("Question")
                .field(&span.to_string())
                .finish(),
            Token::Imply(ref span) => fmt.debug_tuple("Imply").field(&span.to_string()).finish(),
            Token::Not(ref span) => fmt.debug_tuple("Not").field(&span.to_string()).finish(),

            Token::Assert(ref span) => fmt.debug_tuple("Assert").field(&span.to_string()).finish(),
            Token::Else(ref span) => fmt.debug_tuple("Else").field(&span.to_string()).finish(),
            Token::If(ref span) => fmt.debug_tuple("If").field(&span.to_string()).finish(),
            Token::In(ref span) => fmt.debug_tuple("In").field(&span.to_string()).finish(),
            Token::Inherit(ref span) => {
                fmt.debug_tuple("Inherit").field(&span.to_string()).finish()
            }
            Token::Let(ref span) => fmt.debug_tuple("Let").field(&span.to_string()).finish(),
            Token::Or(ref span) => fmt.debug_tuple("Or").field(&span.to_string()).finish(),
            Token::Rec(ref span) => fmt.debug_tuple("Rec").field(&span.to_string()).finish(),
            Token::Then(ref span) => fmt.debug_tuple("Then").field(&span.to_string()).finish(),
            Token::With(ref span) => fmt.debug_tuple("With").field(&span.to_string()).finish(),

            Token::At(ref span) => fmt.debug_tuple("At").field(&span.to_string()).finish(),
            Token::Colon(ref span) => fmt.debug_tuple("Colon").field(&span.to_string()).finish(),
            Token::Comma(ref span) => fmt.debug_tuple("Comma").field(&span.to_string()).finish(),
            Token::Dot(ref span) => fmt.debug_tuple("Dot").field(&span.to_string()).finish(),
            Token::Ellipsis(ref span) => fmt
                .debug_tuple("Ellipsis")
                .field(&span.to_string())
                .finish(),
            Token::Eq(ref span) => fmt.debug_tuple("Eq").field(&span.to_string()).finish(),
            Token::Interpolate(ref span) => fmt
                .debug_tuple("Interpolate")
                .field(&span.to_string())
                .finish(),
            Token::LBrace(ref span) => fmt.debug_tuple("LBrace").field(&span.to_string()).finish(),
            Token::RBrace(ref span) => fmt.debug_tuple("RBrace").field(&span.to_string()).finish(),
            Token::LBracket(ref span) => fmt
                .debug_tuple("LBracket")
                .field(&span.to_string())
                .finish(),
            Token::RBracket(ref span) => fmt
                .debug_tuple("RBracket")
                .field(&span.to_string())
                .finish(),
            Token::LParen(ref span) => fmt.debug_tuple("LParen").field(&span.to_string()).finish(),
            Token::RParen(ref span) => fmt.debug_tuple("RParen").field(&span.to_string()).finish(),
            Token::Semi(ref span) => fmt.debug_tuple("Semi").field(&span.to_string()).finish(),
        }
    }
}

impl<'a> ToSpan for Token<'a> {
    fn to_span(&self) -> Span {
        match *self {
            Token::Eof(ref span) => *span,
            Token::Unknown(_, ref span, _) => *span,
            Token::Whitespace(ref span) => *span,

            Token::Comment(_, _, ref span) => *span,
            Token::Identifier(_, ref span) => *span,
            Token::Null(ref span) => *span,
            Token::Boolean(_, ref span) => *span,
            Token::Float(_, ref span) => *span,
            Token::Integer(_, ref span) => *span,
            Token::Interpolation(_, ref span) => *span,
            Token::Path(_, ref span) => *span,
            Token::PathTemplate(_, ref span) => *span,
            Token::String(_, _, ref span) => *span,
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
