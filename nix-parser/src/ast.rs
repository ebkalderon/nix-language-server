use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::ByteSpan;

use self::tokens::{Ident, IdentPath, Literal};

pub mod tokens;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// A parenthesized expression.
    ///
    /// This type of expression is only used to aid with serialization of the AST back into a token
    /// string.
    Paren(ExprParen),
    /// `foo`
    Attr(IdentPath),
    /// `12`, `4.0`, `false`, `"foo"`, `''bar''`, `./foo/bar`, `null`, `http://www.example.com`
    Literal(Literal),
    /// `[1 2 3 4]`
    List(ExprList),
    /// `{ foo = "hello"; bar = 123; }`
    Set(ExprSet),

    /// `-12`
    /// `!15.0`
    Unary(ExprUnary),
    /// `1 + 1`, `true && false`, `"foo" ++ hello + "bar"`, `"foo ${hello} bar"`
    Binary(ExprBinary),

    /// `let { foo = "bar"; }`
    Let(ExprLet),
    /// `rec { foo = "bar"; }`
    Rec(ExprRec),
    /// `x.y`
    Proj(ExprProj),

    /// `if true then "success" else "failure"`
    If(ExprIf),
    /// `foo.bar or "failed"`
    Or(ExprOr),
    /// `assert true != false; true`
    Assert(ExprAssert),
    /// `with foo; foo.attr`
    With(ExprWith),

    /// `let foo = "bar"; in foo`
    LetIn(ExprLetIn),
    /// `foo: 1 + 2`, `{ x, y }: x + y`, `{ x, y } @ foo: x + y`
    FnDecl(ExprFnDecl),
    /// `foo one`
    FnApp(ExprFnApp),
}

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Expr::Paren(ref e) => write!(fmt, "{}", e),
            Expr::Attr(ref e) => write!(fmt, "{}", e),
            Expr::Literal(ref e) => write!(fmt, "{}", e),
            Expr::List(ref e) => write!(fmt, "{}", e),
            Expr::Set(ref e) => write!(fmt, "{}", e),

            Expr::Unary(ref e) => write!(fmt, "{}", e),
            Expr::Binary(ref e) => write!(fmt, "{}", e),

            Expr::Let(ref e) => write!(fmt, "{}", e),
            Expr::Rec(ref e) => write!(fmt, "{}", e),
            Expr::Proj(ref e) => write!(fmt, "{}", e),

            Expr::If(ref e) => write!(fmt, "{}", e),
            Expr::Or(ref e) => write!(fmt, "{}", e),
            Expr::Assert(ref e) => write!(fmt, "{}", e),
            Expr::With(ref e) => write!(fmt, "{}", e),

            Expr::LetIn(ref e) => write!(fmt, "{}", e),
            Expr::FnDecl(ref e) => write!(fmt, "{}", e),
            Expr::FnApp(ref e) => write!(fmt, "{}", e),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprParen {
    expr: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprParen {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "({})", self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprList {
    elems: Vec<Expr>,
    span: ByteSpan,
}

impl Display for ExprList {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let elems: Vec<_> = self.elems.iter().map(ToString::to_string).collect();
        write!(fmt, "[{}]", elems.join(" "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprSet {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl Display for ExprSet {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "{{{}}}", binds.join(" "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    /// The unary `-` operator.
    Neg,
    /// The unary `!` operator.
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            UnaryOp::Neg => fmt.write_str("-"),
            UnaryOp::Not => fmt.write_str("!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprUnary {
    op: UnaryOp,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprUnary {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}{}", self.op, self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
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
    Impl,
}

impl Display for BinaryOp {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            BinaryOp::Add => fmt.write_str("+"),
            BinaryOp::Sub => fmt.write_str("-"),
            BinaryOp::Mul => fmt.write_str("*"),
            BinaryOp::Div => fmt.write_str("/"),
            BinaryOp::Eq => fmt.write_str("=="),
            BinaryOp::NotEq => fmt.write_str("!="),
            BinaryOp::LessThan => fmt.write_str("<"),
            BinaryOp::LessThanEq => fmt.write_str("<="),
            BinaryOp::GreaterThan => fmt.write_str(">"),
            BinaryOp::GreaterThanEq => fmt.write_str(">="),
            BinaryOp::And => fmt.write_str("&&"),
            BinaryOp::Or => fmt.write_str("||"),
            BinaryOp::Concat => fmt.write_str("++"),
            BinaryOp::Update => fmt.write_str("//"),
            BinaryOp::HasAttr => fmt.write_str("?"),
            BinaryOp::Impl => fmt.write_str("->"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprBinary {
    op: BinaryOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprBinary {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Bind {
    Simple {
        name: IdentPath,
        expr: Box<Expr>,
        span: ByteSpan,
    },
    Inherit {
        names: Vec<Ident>,
        span: ByteSpan,
    },
    InheritExpr {
        expr: Box<Expr>,
        names: Vec<Ident>,
        span: ByteSpan,
    },
}

impl Display for Bind {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Bind::Simple {
                ref name, ref expr, ..
            } => write!(fmt, "{} = {};", name, expr),
            Bind::Inherit { ref names, .. } => {
                let names: Vec<_> = names.iter().map(ToString::to_string).collect();
                write!(fmt, "inherit {};", names.join(" "))
            }
            Bind::InheritExpr {
                ref expr,
                ref names,
                ..
            } => {
                let names: Vec<_> = names.iter().map(ToString::to_string).collect();
                write!(fmt, "inherit ({}) {};", expr, names.join(" "))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprLet {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl Display for ExprLet {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "let {{{}}}", binds.join(" "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprRec {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl Display for ExprRec {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "rec {{{}}}", binds.join(" "))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprProj {
    expr: Box<Expr>,
    attr: Ident,
    span: ByteSpan,
}

impl Display for ExprProj {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}.{}", self.expr, self.attr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprIf {
    cond: Box<Expr>,
    body: Box<Expr>,
    fallback: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprIf {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(
            fmt,
            "if {} then {} else {}",
            self.cond, self.body, self.fallback
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprOr {
    expr: Box<Expr>,
    fallback: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprOr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} or {}", self.expr, self.fallback)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprAssert {
    cond: Box<Expr>,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprAssert {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "assert {}; {}", self.cond, self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprWith {
    with: Box<Expr>,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprWith {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "with {}; {}", self.with, self.expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Formal {
    Simple(Ident),
    Default {
        name: Ident,
        default: Box<Expr>,
        span: ByteSpan,
    },
    Ellipsis(ByteSpan),
}

impl Display for Formal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Formal::Simple(ref name) => write!(fmt, "{}", name),
            Formal::Default {
                ref name,
                ref default,
                ..
            } => write!(fmt, "{} ? {}", name, default),
            Formal::Ellipsis(_) => fmt.write_str("..."),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprLetIn {
    binds: Vec<Bind>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprLetIn {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "let {} in {}", binds.join(" "), self.body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprFnDecl {
    Simple(SimpleFnDecl),
    Formals(FormalsFnDecl),
    FormalsExtra(FormalsExtraFnDecl),
}

impl Display for ExprFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            ExprFnDecl::Simple(ref s) => write!(fmt, "{}", s),
            ExprFnDecl::Formals(ref f) => write!(fmt, "{}", f),
            ExprFnDecl::FormalsExtra(ref e) => write!(fmt, "{}", e),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SimpleFnDecl {
    name: Ident,
    body: Box<Expr>,
    span: ByteSpan,
}

impl Display for SimpleFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}: {}", self.name, self.body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalsFnDecl {
    formals: Vec<Formal>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl Display for FormalsFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let formals: Vec<_> = self.formals.iter().map(ToString::to_string).collect();
        write!(fmt, "{{{}}}: {}", formals.join(", "), self.body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalsExtraFnDecl {
    extra: Ident,
    is_prefix: bool,
    formals: Vec<Formal>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl Display for FormalsExtraFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let f: Vec<_> = self.formals.iter().map(ToString::to_string).collect();
        if self.is_prefix {
            write!(fmt, "{} @ {{{}}}: {}", self.extra, f.join(", "), self.body)
        } else {
            write!(fmt, "{{{}}} @ {}: {}", f.join(", "), self.extra, self.body)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprFnApp {
    name: Ident,
    body: Box<Expr>,
    span: ByteSpan,
}

impl Display for ExprFnApp {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} {}", self.name, self.body)
    }
}
