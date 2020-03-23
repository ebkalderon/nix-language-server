//! Types of tokens and token kinds.

use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Range;

use codespan::Span;
#[cfg(feature = "serialization")]
use serde::{Deserialize, Serialize};

/// A token with span information.
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
pub struct Token {
    /// The kind of token.
    pub kind: TokenKind,
    /// The span of the token.
    pub span: Span,
}

impl Token {
    /// Creates a new `Token` from the given `kind` and `span`.
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    /// Returns whether the given token is a kind of trivia, i.e. whitespace and comments.
    pub fn is_trivia(&self) -> bool {
        self.kind.is_trivia()
    }

    /// Returns an object which implements `Display` which can be used to display a token, given a
    /// slice of source text.
    pub const fn display<'s>(&self, source: &'s str) -> DisplayToken<'s> {
        DisplayToken {
            source,
            kind: self.kind,
            span: self.span,
        }
    }
}

/// Helper struct for printing tokens with `format!` and `{}`.
pub struct DisplayToken<'s> {
    source: &'s str,
    kind: TokenKind,
    span: Span,
}

impl Debug for DisplayToken<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct(stringify!(Token))
            .field("kind", &self.kind)
            .field("span", &self.span)
            .finish()
    }
}

impl Display for DisplayToken<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let range = Range::from(self.span);
        write!(
            f,
            "Kind: {:?}, Span: {}, Text: {:?}",
            self.kind, self.span, &self.source[range]
        )
    }
}

/// A list specifying all possible tokens.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
pub enum TokenKind {
    /// `# foo`
    LineComment,
    /// `/* foo */`
    BlockComment { terminated: bool },
    /// Spaces, tabs, and newlines
    Whitespace,

    /// `foo`, `_`, `let`
    Ident,
    /// Any literal value
    Literal { kind: LiteralKind },
    /// `"`, `''`
    StringTerm { kind: StringKind },
    /// Text span enclosed in one or more `StringTerm`s
    StringLiteral,

    // Operators
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    IsEq,
    /// `!=`
    NotEq,
    /// `<`
    LessThan,
    /// `<=`
    LessThanEq,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEq,
    /// `&&`
    LogicalAnd,
    /// `||`
    LogicalOr,
    /// `++`
    Concat,
    /// `//`
    Update,
    /// `?`
    Question,
    /// `->`
    Imply,
    /// `!`
    Not,

    /// `@`
    At,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `...`
    Ellipsis,
    /// `=`
    Eq,
    /// `${`
    Interpolate,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `;`
    Semi,

    /// Any token unknown to the lexer, e.g. `^`
    Unknown,
}

impl TokenKind {
    /// Returns whether the given token is a kind of trivia, i.e. whitespace and comments.
    pub fn is_trivia(self) -> bool {
        match self {
            TokenKind::LineComment | TokenKind::BlockComment { .. } | TokenKind::Whitespace => true,
            _ => false,
        }
    }

    /// Returns a static description of a `TokenKind` suitable for error reporting.
    pub fn description(self) -> &'static str {
        match self {
            TokenKind::LineComment => "line comment",
            TokenKind::BlockComment { .. } => "block comment",
            TokenKind::Whitespace => "whitespace",

            TokenKind::Ident => "identifier",
            TokenKind::Literal { kind } => match kind {
                LiteralKind::Float => "float literal",
                LiteralKind::Integer => "integer literal",
                LiteralKind::Path { .. } => "path literal",
                LiteralKind::PathTemplate { .. } => "path template literal",
                LiteralKind::Uri => "URI literal",
            },
            TokenKind::StringTerm { kind } => match kind {
                StringKind::Normal => "`\"`",
                StringKind::Indented => "`''`",
            },
            TokenKind::StringLiteral => "string literal",

            TokenKind::Add => "`+`",
            TokenKind::Sub => "`-`",
            TokenKind::Mul => "`*`",
            TokenKind::Div => "`/`",

            TokenKind::IsEq => "`==`",
            TokenKind::NotEq => "`!=`",
            TokenKind::LessThan => "`<`",
            TokenKind::LessThanEq => "`<=`",
            TokenKind::GreaterThan => "`>`",
            TokenKind::GreaterThanEq => "`>=`",
            TokenKind::LogicalAnd => "`&&`",
            TokenKind::LogicalOr => "`||`",
            TokenKind::Concat => "`&&`",
            TokenKind::Update => "`//`",
            TokenKind::Question => "`?`",
            TokenKind::Imply => "`->`",
            TokenKind::Not => "`!`",

            TokenKind::At => "`@`",
            TokenKind::Colon => "`:`",
            TokenKind::Comma => "`,`",
            TokenKind::Dot => "`.`",
            TokenKind::Ellipsis => "`...`",
            TokenKind::Eq => "`=`",
            TokenKind::Interpolate => "`${`",
            TokenKind::OpenBrace => "`{`",
            TokenKind::CloseBrace => "`}`",
            TokenKind::OpenBracket => "`[`",
            TokenKind::CloseBracket => "`]`",
            TokenKind::OpenParen => "`(`",
            TokenKind::CloseParen => "`)`",
            TokenKind::Semi => "`;`",

            TokenKind::Unknown => "<unknown>",
        }
    }
}

/// A list of valid literal values.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
pub enum LiteralKind {
    /// `3.14`
    Float,
    /// `1234`, `00001`
    Integer,
    /// `./foo/bar`, `~/foo/bar`, `/foo/bar`, `foo/bar`
    Path { trailing_slash: bool },
    /// `<nixpkgs>`, `<nixpkgs/foo>`
    PathTemplate { trailing_slash: bool },
    /// `https://github.com/NixOS/nix`
    Uri,
}

/// A list of valid string terminators.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
pub enum StringKind {
    /// A string terminated by double quotes, `"`.
    Normal,
    /// A string terminated by two single quotes, `''`.
    Indented,
}
