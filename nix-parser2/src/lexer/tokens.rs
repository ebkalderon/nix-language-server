//! Types of tokens and token kinds.

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
    pub fn is_trivia(&self) -> bool {
        match *self {
            TokenKind::LineComment | TokenKind::BlockComment { .. } | TokenKind::Whitespace => true,
            _ => false,
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
