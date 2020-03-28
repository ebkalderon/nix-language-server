//! Abstract syntax tree that can be evaluated or type-checked.

use std::borrow::Cow;
use std::fmt::{self, Display, Formatter, Write};
use std::path::PathBuf;

use codespan::Span;
use smol_str::SmolStr;
use url::Url;

/// A valid Nix identifier.
#[derive(Clone, Debug, PartialEq)]
pub struct Ident(SmolStr);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl From<SmolStr> for Ident {
    fn from(s: SmolStr) -> Self {
        Ident(s)
    }
}

impl From<&'_ str> for Ident {
    fn from(s: &'_ str) -> Self {
        Ident(SmolStr::new(s))
    }
}

/// A list of valid literal values.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// `3.14`
    Float(f64),
    /// `1234`, `00001`
    Integer(i64),
    /// `./foo/bar`, `~/foo/bar`, `/foo/bar`, `foo/bar`
    Path(PathBuf),
    /// `<nixpkgs>`, `<nixpkgs/foo>`
    PathTemplate(PathBuf),
    /// `https://github.com/NixOS/nix`
    Uri(Url),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Literal::Float(ref lit) => lit.fmt(f),
            Literal::Integer(ref lit) => lit.fmt(f),
            Literal::Path(ref lit) => lit.display().fmt(f),
            Literal::PathTemplate(ref lit) => write!(f, "<{}>", lit.display()),
            Literal::Uri(ref lit) => lit.fmt(f),
        }
    }
}

/// A Nix expression with span information.
#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    /// The kind of expression.
    pub kind: ExprKind,
    /// The span of the expression.
    pub span: Span,
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.kind, f)
    }
}

/// A list of valid expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    /// A parenthesized expression.
    ///
    /// This expression only exists to aid with pretty-printing ASTs.
    Paren(Box<Expr>),
    /// `foo`
    Ident(Ident),
    /// `12`, `4.0`, `false`, `"foo"`, `''bar''`, `./foo/bar`, `null`, `http://www.example.com`
    Literal(Literal),
    /// `"foo"`, `''bar''`, `"baz ${quux}"`
    String(Vec<StringFragment>),
    /// `[1 2 3 4]`
    List(Vec<Expr>),
    /// `rec { foo = "bar"; bar = 123; }`
    Rec(Vec<Binding>),
    /// `{ foo = "hello"; bar = 123; }`
    Set(Vec<Binding>),

    /// `-12`, `!15.0`
    Unary(Box<ExprUnary>),
    /// `1 + 1`, `true && false`, `[ 1 2 ] ++ [ 3 4 ]`
    Binary(Box<ExprBinary>),

    /// `if true then "success" else "failure"`
    If(Box<ExprIf>),
    /// `let { foo = "hello"; bar = 123; }`
    LegacyLet(Vec<Binding>),
    /// `let foo = "bar"; in foo`
    LetIn(Box<ExprLetIn>),
    /// `foo.bar`, `foo.bar or "true"`
    Proj(Box<ExprProj>),

    /// `assert true != false; true`
    Assert(Box<ExprAssert>),
    /// `with foo; foo.attr`
    With(Box<ExprWith>),

    /// `foo one two`
    Apply(Box<ExprApply>),
    /// `x: x + y`, `{ x, y }: x + y`, `{ x, y, ... }@foo: x + y`
    Lambda(Box<ExprLambda>),

    /// An invalid expression.
    Error,
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            ExprKind::Paren(ref inner) => write!(f, "({})", inner),
            ExprKind::Ident(ref ident) => Display::fmt(ident, f),
            ExprKind::Literal(ref lit) => Display::fmt(lit, f),
            ExprKind::String(ref frags) => {
                write!(f, "\"")?;

                for frag in frags {
                    write!(f, "{}", frag)?;
                }

                write!(f, "\"")
            }
            ExprKind::List(ref elems) => {
                write!(f, "[")?;

                if f.alternate() && !elems.is_empty() {
                    if elems.len() > 2 {
                        writeln!(f)?;
                        for elem in elems {
                            writeln!(f.indent(2), "{:#}", elem)?;
                        }
                    } else {
                        for elem in elems {
                            write!(f, " {:#}", elem)?;
                        }

                        f.write_str(" ")?;
                    }
                } else {
                    for elem in elems {
                        write!(f, " {}", elem)?;
                    }

                    if !elems.is_empty() {
                        f.write_str(" ")?;
                    }
                }

                write!(f, "]")
            }
            ExprKind::Rec(ref binds) => {
                write!(f, "rec ")?;
                display_attr_set(f, &binds)
            }
            ExprKind::Set(ref binds) => display_attr_set(f, &binds),

            ExprKind::Unary(ref e) => Display::fmt(e, f),
            ExprKind::Binary(ref e) => Display::fmt(e, f),

            ExprKind::If(ref e) => Display::fmt(e, f),
            ExprKind::LegacyLet(ref binds) => {
                write!(f, "let ")?;
                display_attr_set(f, &binds)
            }
            ExprKind::LetIn(ref e) => Display::fmt(e, f),
            ExprKind::Proj(ref e) => Display::fmt(e, f),

            ExprKind::Assert(ref e) => Display::fmt(e, f),
            ExprKind::With(ref e) => Display::fmt(e, f),

            ExprKind::Apply(ref e) => Display::fmt(e, f),
            ExprKind::Lambda(ref e) => Display::fmt(e, f),

            ExprKind::Error => f.write_str("<error>"),
        }
    }
}

/// Renders an attribute set given a set of attribute bindings.
fn display_attr_set(f: &mut Formatter, binds: &[Binding]) -> fmt::Result {
    write!(f, "{{")?;

    if f.alternate() && !binds.is_empty() {
        if binds.len() > 1 {
            writeln!(f)?;
            for bind in binds {
                writeln!(f.indent(2), "{:#}", bind)?;
            }
        } else if let Some(ref bind) = binds.first() {
            match &bind.kind {
                BindingKind::Inherit(..) => write!(f, " {:#} ", bind)?,
                BindingKind::Simple(..) => {
                    writeln!(f)?;
                    writeln!(f.indent(2), "{:#}", bind)?;
                }
            }
        }
    } else {
        for bind in binds {
            write!(f, " {}", bind)?;
        }

        if !binds.is_empty() {
            f.write_str(" ")?;
        }
    }

    write!(f, "}}")
}

/// A list of all possible components of a string.
#[derive(Clone, Debug, PartialEq)]
pub enum StringFragment {
    /// An unescaped string literal.
    Literal(Cow<'static, str>, Span),
    /// A string interpolation.
    ///
    /// The `Span` field includes the `${` and `}` delimiters.
    Interpolation(Expr, Span),
}

impl Display for StringFragment {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            StringFragment::Literal(ref lit, _) => write!(f, "{}", lit),
            StringFragment::Interpolation(ref e, _) => write!(f, "${{{}}}", e),
        }
    }
}

/// An attribute path used in set bindings and projections.
///
/// # Examples
///
/// ```nix
/// foo."bar".${baz}
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct AttrPath {
    /// List of path segments, each separated by a dot character.
    pub segments: Vec<AttrSegment>,
    /// The span of the attribute path.
    pub span: Span,
}

impl Display for AttrPath {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut segments = self.segments.iter();

        if let Some(ref first) = segments.next() {
            write!(f, "{}", first)?;
        } else {
            return Ok(());
        }

        for s in segments {
            write!(f, ".{}", s)?;
        }

        Ok(())
    }
}

/// A list of valid attribute path segments.
#[derive(Clone, Debug, PartialEq)]
pub enum AttrSegment {
    /// An identifier segment.
    Ident(Ident, Span),
    /// A bare interpolation segment.
    Interpolation(Expr, Span),
    /// A string segment.
    String(Vec<StringFragment>, Span),
}

impl Display for AttrSegment {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            AttrSegment::Ident(ref ident, _) => Display::fmt(ident, f),
            AttrSegment::Interpolation(ref e, _) => write!(f, "${{{}}}", e),
            AttrSegment::String(ref frags, _) => {
                write!(f, "\"")?;

                for frag in frags {
                    write!(f, "{}", frag)?;
                }

                write!(f, "\"")
            }
        }
    }
}

/// A key-value binding used in sets and let-in expressions, among others.
#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    /// The kind of binding.
    pub kind: BindingKind,
    /// The span of the binding, including the trailing semicolon.
    pub span: Span,
}

impl Display for Binding {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#};", self.kind)
        } else {
            write!(f, "{};", self.kind)
        }
    }
}

/// A list of all valid kinds of bindings.
#[derive(Clone, Debug, PartialEq)]
pub enum BindingKind {
    /// A simple key-value binding, separated by an equal sign.
    Simple(AttrPath, Expr),
    /// An `inherit` binding, with an optional expression to inherit from.
    Inherit(Option<Expr>, Vec<Ident>),
}

impl Display for BindingKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            BindingKind::Simple(ref k, ref v) => {
                if f.alternate() {
                    write!(f, "{} = {:#}", k, v)
                } else {
                    write!(f, "{} = {}", k, v)
                }
            }
            BindingKind::Inherit(ref from, ref names) => {
                f.write_str("inherit")?;

                if let Some(ref from) = from {
                    write!(f, " ({})", from)?;
                }

                for name in names {
                    write!(f, " {}", name)?;
                }

                Ok(())
            }
        }
    }
}

/// A list of valid unary operators.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    /// The unary `-` operator.
    Neg,
    /// The unary `!` operator.
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            UnaryOp::Neg => f.write_str("-"),
            UnaryOp::Not => f.write_str("!"),
        }
    }
}

/// A unary expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprUnary {
    /// The unary operator to apply.
    pub op: UnaryOp,
    /// The base expression.
    pub expr: Expr,
}

impl Display for ExprUnary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}{:#}", self.op, self.expr)
        } else {
            write!(f, "{}{}", self.op, self.expr)
        }
    }
}

/// A list of valid binary operators.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    /// The binary `+` operator.
    Add,
    /// The binary `-` operator.
    Sub,
    /// The binary `*` operator.
    Mul,
    /// The binary `/` operator.
    Div,
    /// The binary `==` operator.
    Eq,
    /// The binary `!=` operator.
    NotEq,
    /// The binary `<` operator.
    LessThan,
    /// The binary `<=` operator.
    LessThanEq,
    /// The binary `>` operator.
    GreaterThan,
    /// The binary `>=` operator.
    GreaterThanEq,
    /// The binary `&&` operator.
    And,
    /// The binary `||` operator.
    Or,
    /// The binary `++` operator.
    Concat,
    /// The binary `//` operator.
    Update,
    /// The binary `?` operator.
    HasAttr,
    /// The binary `->` operator.
    Imply,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            BinaryOp::Add => f.write_str("+"),
            BinaryOp::Sub => f.write_str("-"),
            BinaryOp::Mul => f.write_str("*"),
            BinaryOp::Div => f.write_str("/"),
            BinaryOp::Eq => f.write_str("=="),
            BinaryOp::NotEq => f.write_str("!="),
            BinaryOp::LessThan => f.write_str("<"),
            BinaryOp::LessThanEq => f.write_str("<="),
            BinaryOp::GreaterThan => f.write_str(">"),
            BinaryOp::GreaterThanEq => f.write_str(">="),
            BinaryOp::And => f.write_str("&&"),
            BinaryOp::Or => f.write_str("||"),
            BinaryOp::Concat => f.write_str("++"),
            BinaryOp::Update => f.write_str("//"),
            BinaryOp::HasAttr => f.write_str("?"),
            BinaryOp::Imply => f.write_str("->"),
        }
    }
}

/// A binary expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprBinary {
    /// The binary operator to apply.
    pub op: BinaryOp,
    /// The left-hand side of the expression.
    pub lhs: Expr,
    /// The right-hand side of the expression.
    pub rhs: Expr,
}

impl Display for ExprBinary {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#} {} {:#}", self.lhs, self.op, self.rhs)
        } else {
            write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
        }
    }
}

/// An `if` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprIf {
    /// The condition to evaluate.
    pub cond: Expr,
    /// The expression to evaluate if `cond` evaluates to `true`.
    pub body: Expr,
    /// The expression to evaluate if `cond` evaluates to `false`.
    pub fallback: Expr,
}

impl Display for ExprIf {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            writeln!(f, "if {} then", self.cond)?;
            writeln!(f.indent(2), "{:#}", self.body)?;
            writeln!(f, "else")?;
            write!(f.indent(2), "{:#}", self.fallback)
        } else {
            write!(
                f,
                "if {} then {} else {}",
                self.cond, self.body, self.fallback
            )
        }
    }
}

/// A `let-in` block expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprLetIn {
    /// The scoped bindings to create.
    pub bindings: Vec<Binding>,
    /// The expression to evaluate with the above bindings in scope.
    pub body: Expr,
}

impl Display for ExprLetIn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("let")?;

        if f.alternate() {
            writeln!(f)?;

            for bind in &self.bindings {
                writeln!(f.indent(2), "{:#}", bind)?;
            }

            writeln!(f, "in")?;
            write!(f.indent(2), "{:#}", self.body)
        } else {
            for bind in &self.bindings {
                write!(f, " {}", bind)?;
            }

            write!(f, " in {}", self.body)
        }
    }
}

/// A field projection.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprProj {
    /// The base expression to evaluate.
    pub base: Expr,
    /// The attribute path to project from the base.
    pub path: AttrPath,
    /// A fallback value to use if the projection fails.
    ///
    /// # Examples
    ///
    /// ```nix
    /// { foo = 1; bar = 2; }.baz or 3
    ///                           ^^^^
    /// ```
    pub or_default: Option<Expr>,
}

impl Display for ExprProj {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}", self.base)?;
        } else {
            write!(f, "{}", self.base)?;
        }

        write!(f, ".{}", self.path)?;

        if let Some(ref default) = &self.or_default {
            if f.alternate() {
                write!(f, " or {:#}", default)?;
            } else {
                write!(f, " or {}", default)?;
            }
        }

        Ok(())
    }
}

/// An `assert` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprAssert {
    /// The condition to assert.
    pub cond: Expr,
    /// The expression to evaluate if the assertion holds.
    pub body: Expr,
}

impl Display for ExprAssert {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "assert {};\n{:#}", self.cond, self.body)
        } else {
            write!(f, "assert {}; {}", self.cond, self.body)
        }
    }
}

/// A `with` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprWith {
    /// The expression to evaluate and bring its symbols into scope.
    pub with: Expr,
    /// The body expression to evaluate.
    pub body: Expr,
}

impl Display for ExprWith {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "with {};\n{:#}", self.with, self.body)
        } else {
            write!(f, "with {}; {}", self.with, self.body)
        }
    }
}

/// A function application.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprApply {
    /// The function to apply.
    pub function: Expr,
    /// The argument to be supplied.
    pub argument: Expr,
}

impl Display for ExprApply {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#} {:#}", self.function, self.argument)
        } else {
            write!(f, "{} {}", self.function, self.argument)
        }
    }
}

/// A lambda expression.
#[derive(Clone, Debug, PartialEq)]
pub struct ExprLambda {
    /// The lambda argument pattern.
    pub pattern: Pattern,
    /// The lambda function body.
    pub body: Expr,
}

impl Display for ExprLambda {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            match &self.pattern.kind {
                pat @ PatternKind::Ident(_) => write!(f, "{:#} {:#}", pat, self.body),
                pat @ PatternKind::Set(_) => write!(f, "{:#}\n\n{:#}", pat, self.body),
            }
        } else {
            write!(f, "{} {}", self.pattern, self.body)
        }
    }
}

/// A lambda argument pattern.
#[derive(Clone, Debug, PartialEq)]
pub struct Pattern {
    /// The kind of pattern.
    pub kind: PatternKind,
    /// The span of the pattern, including the trailing `:` character.
    pub span: Span,
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#}:", self.kind)
        } else {
            write!(f, "{}:", self.kind)
        }
    }
}

/// A list of all valid lambda argument patterns.
#[derive(Clone, Debug, PartialEq)]
pub enum PatternKind {
    /// An identifier pattern, e.g. `x:`
    Ident(Ident),
    /// A set pattern with formal arguments, e.g. `{ x, y ? true, ... }@foo:`
    Set(SetPattern),
}

impl Display for PatternKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            PatternKind::Ident(ref pat) => Display::fmt(pat, f),
            PatternKind::Set(ref pat) => Display::fmt(pat, f),
        }
    }
}

/// A set pattern with formal arguments.
#[derive(Clone, Debug, PartialEq)]
pub struct SetPattern {
    /// The formal arguments in the pattern.
    pub args: Vec<FormalArg>,
    /// Whether the set pattern includes an ellipsis.
    pub ellipsis: bool,
    /// The binding to assign for overflow arguments, if any.
    pub bind: Option<Ident>,
}

impl Display for SetPattern {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(ref bind) = &self.bind {
            write!(f, "{}@", bind)?;
        }

        write!(f, "{{")?;

        if f.alternate() {
            if self.args.len() > 1 || !self.args.is_empty() && self.ellipsis {
                writeln!(f)?;

                for arg in &self.args {
                    writeln!(f.indent(2), "{:#},", arg)?;
                }

                if self.ellipsis {
                    writeln!(f.indent(2), ", ...")?;
                }
            } else if let Some(ref arg) = self.args.first() {
                write!(f, " {:#}", arg)?;
                if self.ellipsis {
                    write!(f, ", ... ")?;
                } else {
                    write!(f, " ")?;
                }
            }
        } else {
            let mut args = self.args.iter();

            if let Some(ref formal) = args.next() {
                write!(f, "{}", formal)?;
            }

            for formal in args {
                write!(f, ", {}", formal)?;
            }

            if self.ellipsis {
                if self.args.is_empty() {
                    write!(f, "...")?;
                } else {
                    write!(f, ", ...")?;
                }
            }
        }

        write!(f, "}}")
    }
}

/// A formal argument used in set patterns.
#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    /// The name of the argument.
    pub name: Ident,
    /// A fallback value to use if the argument is unspecified.
    ///
    /// # Examples
    ///
    /// ```nix
    /// foo ? { bar = "baz"; },
    /// ```
    pub default: Option<Expr>,
    /// The span of the formal argument.
    pub span: Span,
}

impl Display for FormalArg {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;

        if let Some(ref default) = &self.default {
            if f.alternate() {
                write!(f, " or {:#}", default)?;
            } else {
                write!(f, " or {}", default)?;
            }
        }

        Ok(())
    }
}

/// A trait which extends the functionality of [`std::fmt::Formatter`].
///
/// [`std::fmt::Formatter`]: https://doc.rust-lang.org/std/fmt/struct.Formatter.html
trait FormatterExt<'a> {
    /// Indents the given formatter with the given number of spaces.
    fn indent<'b>(&'b mut self, level: usize) -> Indented<'b, 'a>;
}

impl<'a> FormatterExt<'a> for Formatter<'a> {
    fn indent<'b>(&'b mut self, level: usize) -> Indented<'b, 'a> {
        Indented {
            fmt: self,
            level,
            newline: true,
        }
    }
}

/// Formatter which indents each line by a certain amount.
struct Indented<'a, 'b: 'a> {
    fmt: &'a mut Formatter<'b>,
    level: usize,
    newline: bool,
}

impl<'a, 'b: 'a> Write for Indented<'a, 'b> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            if c == '\n' {
                self.fmt.write_char(c)?;
                self.newline = true;
                continue;
            }

            if self.newline {
                write!(self.fmt, "{:indent$}", "", indent = self.level)?;
            }

            self.fmt.write_char(c)?;
            self.newline = false;
        }

        Ok(())
    }
}
