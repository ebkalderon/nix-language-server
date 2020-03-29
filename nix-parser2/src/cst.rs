//! Lossless concrete syntax tree.

use codespan::Span;
use nix_errors::Errors;
use nix_lexer::{LiteralKind, StringKind, TokenKind};
use rowan::{GreenNodeBuilder, Language, SmolStr, TextRange};

use crate::error::Error;
use crate::ToSpan;

/// A node in a concrete syntax tree.
///
/// These nodes correspond to syntactic constructs such as bindings, attribute paths, formal
/// argument patterns, and expressions.
pub type SyntaxNode = rowan::SyntaxNode<NixLanguage>;
/// A token with span information.
///
/// These tokens correspond directly to spans in the source text.
pub type SyntaxToken = rowan::SyntaxToken<NixLanguage>;
/// A type that represents either a `SyntaxNode` or a `SyntaxToken`.
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl ToSpan for TextRange {
    fn to_span(&self) -> Span {
        let start = self.start().to_usize() as u32;
        let end = self.end().to_usize() as u32;
        Span::new(start, end)
    }
}

impl ToSpan for SyntaxNode {
    fn to_span(&self) -> Span {
        self.text_range().to_span()
    }
}

/// A marker type for identifying Nix concrete syntax trees.
#[doc(hidden)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum NixLanguage {}

impl Language for NixLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// A list specifying all possible syntax elements.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(u16)]
pub enum SyntaxKind {
    // Tokens
    /// `# foo`
    LineComment = 0,
    /// `/* foo */`
    BlockComment,
    /// Spaces, tabs, and newlines
    Whitespace,
    /// `foo`, `_`, `let`
    Ident,
    /// `3.14`
    Float,
    /// `1234`, `00001`
    Integer,
    /// `./foo/bar`, `~/foo/bar`, `/foo/bar`, `foo/bar`
    Path,
    /// `<nixpkgs>`, `<nixpkgs/foo>`
    PathTemplate,
    /// `https://github.com/NixOS/nix`
    Uri,
    /// `"`
    StringTermNormal,
    /// `''`
    StringTermIndented,
    /// Text span enclosed in one or more `StringTerm`s
    StringLiteral,
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
    OpenInterpolate,
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

    // Compound nodes
    /// `foo one two`
    ExprApply,
    /// `assert true != false; true`
    ExprAssert,
    /// `1 + 1`, `true && false`, `[ 1 2 ] ++ [ 3 4 ]`
    ExprBinary,
    /// An invalid expression.
    ExprError,
    /// `foo`, `true`, `null`
    ExprIdent,
    /// `if true then "success" else "failure"`
    ExprIf,
    /// `x: x + y`, `{ x, y }: x + y`, `{ x, y, ... }@foo: x + y`
    ExprLambda,
    /// `let { foo = "hello"; body = foo; }`
    ExprLegacyLet,
    /// `let foo = "bar"; in foo`
    ExprLetIn,
    /// `[ 1 2 3 4 ]`
    ExprList,
    /// `12`, `4.0`, `./foo/bar`, `http://www.example.com`
    ExprLiteral,
    /// `(foo)`
    ExprParen,
    /// `foo.bar`, `foo.bar or "true"`
    ExprProj,
    /// `rec { foo = "bar"; bar = 123; }`
    ExprRec,
    /// `{ foo = "hello"; bar = 123; }`
    ExprSet,
    /// `"foo"`, `''bar''`, `"baz ${quux}"`
    ExprString,
    /// `-12`, `!15.0`
    ExprUnary,
    /// `with foo; foo.attr`
    ExprWith,
    /// `foo."bar".${baz}`
    AttrPath,
    /// `${foo}`
    BareInterpolation,
    /// `foo = bar;`
    BindingSimple,
    /// `inherit foo bar;`
    BindingInherit,
    /// `inherit (expr) foo bar;`
    BindingInheritFrom,
    /// `foo`, `foo ? { bar = "baz"; }`
    FormalArg,
    /// `or "foo"`
    OrDefault,
    /// `x:`, `{ x }:`
    Pattern,
    /// `{ x, y, ... }@foo:`
    ///  ^^^^^^^^^^^^^
    SetPattern,
    /// `{ x }@foo:`
    ///       ^^^^
    SetPatternBind,
    /// `${foo}`
    StringInterpolation,
    /// Top-most node of the concrete syntax tree.
    Root,
}

impl SyntaxKind {
    /// Returns whether the given token is a kind of trivia, i.e. whitespace and comments.
    #[inline]
    pub fn is_trivia(self) -> bool {
        self.is_whitespace() || self.is_comment()
    }

    /// Returns whether the given token is a comment.
    #[inline]
    pub fn is_comment(self) -> bool {
        match self {
            SyntaxKind::LineComment | SyntaxKind::BlockComment => true,
            _ => false,
        }
    }

    /// Returns whether the given token is whitespace.
    #[inline]
    pub fn is_whitespace(self) -> bool {
        match self {
            SyntaxKind::Whitespace => true,
            _ => false,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(token: TokenKind) -> Self {
        match token {
            TokenKind::LineComment => SyntaxKind::LineComment,
            TokenKind::BlockComment { .. } => SyntaxKind::BlockComment,
            TokenKind::Whitespace => SyntaxKind::Whitespace,

            TokenKind::Ident => SyntaxKind::Ident,
            TokenKind::Literal { kind } => match kind {
                LiteralKind::Float => SyntaxKind::Float,
                LiteralKind::Integer => SyntaxKind::Integer,
                LiteralKind::Path { .. } => SyntaxKind::Path,
                LiteralKind::PathTemplate { .. } => SyntaxKind::PathTemplate,
                LiteralKind::Uri => SyntaxKind::Uri,
            },
            TokenKind::StringTerm { kind } => match kind {
                StringKind::Normal => SyntaxKind::StringTermNormal,
                StringKind::Indented => SyntaxKind::StringTermIndented,
            },
            TokenKind::StringLiteral => SyntaxKind::StringLiteral,

            TokenKind::Add => SyntaxKind::Add,
            TokenKind::Sub => SyntaxKind::Sub,
            TokenKind::Mul => SyntaxKind::Mul,
            TokenKind::Div => SyntaxKind::Div,
            TokenKind::IsEq => SyntaxKind::IsEq,
            TokenKind::NotEq => SyntaxKind::NotEq,
            TokenKind::LessThan => SyntaxKind::LessThan,
            TokenKind::LessThanEq => SyntaxKind::LessThan,
            TokenKind::GreaterThan => SyntaxKind::GreaterThan,
            TokenKind::GreaterThanEq => SyntaxKind::GreaterThanEq,
            TokenKind::LogicalAnd => SyntaxKind::LogicalAnd,
            TokenKind::LogicalOr => SyntaxKind::LogicalOr,
            TokenKind::Concat => SyntaxKind::Concat,
            TokenKind::Update => SyntaxKind::Update,
            TokenKind::Question => SyntaxKind::Question,
            TokenKind::Imply => SyntaxKind::Imply,
            TokenKind::Not => SyntaxKind::Not,

            TokenKind::At => SyntaxKind::At,
            TokenKind::Colon => SyntaxKind::Colon,
            TokenKind::Comma => SyntaxKind::Comma,
            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Ellipsis => SyntaxKind::Ellipsis,
            TokenKind::Eq => SyntaxKind::Eq,
            TokenKind::Interpolate => SyntaxKind::OpenInterpolate,
            TokenKind::OpenBrace => SyntaxKind::OpenBrace,
            TokenKind::CloseBrace => SyntaxKind::CloseBrace,
            TokenKind::OpenBracket => SyntaxKind::OpenBracket,
            TokenKind::CloseBracket => SyntaxKind::CloseBracket,
            TokenKind::OpenParen => SyntaxKind::OpenParen,
            TokenKind::CloseParen => SyntaxKind::CloseParen,
            TokenKind::Semi => SyntaxKind::Semi,

            TokenKind::Unknown => SyntaxKind::Unknown,
        }
    }
}

/// Constructs a new concrete syntax tree node-by-node.
#[derive(Debug, Default)]
pub(crate) struct CstBuilder {
    errors: Errors<Error>,
    inner: GreenNodeBuilder<'static>,
}

impl CstBuilder {
    /// Constructs a new, empty `CstBuilder`.
    pub fn new() -> Self {
        CstBuilder::default()
    }

    /// Creates a new `SyntaxNode` and makes it current.
    pub fn start_node(&mut self, kind: SyntaxKind) {
        let kind = NixLanguage::kind_to_raw(kind);
        self.inner.start_node(kind)
    }

    /// Attaches a new token to the current branch.
    pub fn token<T: Into<SmolStr>>(&mut self, kind: TokenKind, text: T) {
        let kind = NixLanguage::kind_to_raw(SyntaxKind::from(kind));
        self.inner.token(kind, text.into())
    }

    /// Finishes the current branch and restores the previous branch as current.
    pub fn finish_node(&mut self) {
        self.inner.finish_node();
    }

    /// Appends an error to the error stack.
    pub fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    /// Returns the finished concrete syntax tree and a stack of parse errors, if any.
    pub fn finish(self) -> (SyntaxNode, Errors<Error>) {
        let green = self.inner.finish();
        (SyntaxNode::new_root(green), self.errors)
    }
}
