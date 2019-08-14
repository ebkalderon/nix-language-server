use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::ByteSpan;

use self::tokens::{Comment, Ident, IdentPath, Literal};

pub mod tokens;

/// A source file with a top-level doc comment.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceFile {
    comment: Option<Comment>,
    expr: Box<Expr>,
}

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

#[derive(Clone, Debug)]
pub struct ExprParen {
    expr: Box<Expr>,
    span: ByteSpan,
}

impl ExprParen {
    pub fn new(expr: Box<Expr>, span: ByteSpan) -> Self {
        ExprParen { expr, span }
    }
}

impl Display for ExprParen {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "({})", self.expr)
    }
}

impl PartialEq for ExprParen {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

#[derive(Clone, Debug)]
pub struct ExprList {
    elems: Vec<Expr>,
    span: ByteSpan,
}

impl ExprList {
    pub fn new(elems: Vec<Expr>, span: ByteSpan) -> Self {
        ExprList { elems, span }
    }
}

impl Display for ExprList {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let elems: Vec<_> = self.elems.iter().map(ToString::to_string).collect();
        write!(fmt, "[{}]", elems.join(" "))
    }
}

impl PartialEq for ExprList {
    fn eq(&self, other: &Self) -> bool {
        self.elems == other.elems
    }
}

#[derive(Clone, Debug)]
pub struct ExprSet {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl ExprSet {
    pub fn new(binds: Vec<Bind>, span: ByteSpan) -> Self {
        ExprSet { binds, span }
    }
}

impl Display for ExprSet {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "{{{}}}", binds.join(" "))
    }
}

impl PartialEq for ExprSet {
    fn eq(&self, other: &Self) -> bool {
        self.binds == other.binds
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

#[derive(Clone, Debug)]
pub struct ExprUnary {
    op: UnaryOp,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl ExprUnary {
    pub fn new(op: UnaryOp, expr: Box<Expr>, span: ByteSpan) -> Self {
        ExprUnary { op, expr, span }
    }
}

impl Display for ExprUnary {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}{}", self.op, self.expr)
    }
}

impl PartialEq for ExprUnary {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.expr == other.expr
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

#[derive(Clone, Debug)]
pub struct ExprBinary {
    op: BinaryOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    span: ByteSpan,
}

impl ExprBinary {
    pub fn new(op: BinaryOp, lhs: Box<Expr>, rhs: Box<Expr>, span: ByteSpan) -> Self {
        ExprBinary { op, lhs, rhs, span }
    }
}

impl Display for ExprBinary {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

impl PartialEq for ExprBinary {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.lhs == other.lhs && self.rhs == other.rhs
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Bind {
    Simple(BindSimple),
    Inherit(BindInherit),
    InheritExpr(BindInheritExpr),
}

impl Display for Bind {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Bind::Simple(ref b) => write!(fmt, "{}", b),
            Bind::Inherit(ref b) => write!(fmt, "{}", b),
            Bind::InheritExpr(ref b) => write!(fmt, "{}", b),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BindSimple {
    comment: Option<Comment>,
    name: IdentPath,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl BindSimple {
    pub fn new(comment: Option<Comment>, name: IdentPath, expr: Box<Expr>, span: ByteSpan) -> Self {
        BindSimple {
            comment,
            name,
            expr,
            span,
        }
    }
}

impl Display for BindSimple {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if let Some(ref comment) = self.comment {
            write!(fmt, "{}{} = {};", comment, self.name, self.expr)
        } else {
            write!(fmt, "{} = {};", self.name, self.expr)
        }
    }
}

impl PartialEq for BindSimple {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.expr == other.expr && self.comment == other.comment
    }
}

#[derive(Clone, Debug)]
pub struct BindInherit {
    names: Vec<Ident>,
    span: ByteSpan,
}

impl BindInherit {
    pub fn new(names: Vec<Ident>, span: ByteSpan) -> Self {
        BindInherit { names, span }
    }
}

impl Display for BindInherit {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let names: Vec<_> = self.names.iter().map(ToString::to_string).collect();
        write!(fmt, "inherit {};", names.join(" "))
    }
}

impl PartialEq for BindInherit {
    fn eq(&self, other: &Self) -> bool {
        self.names == other.names
    }
}

#[derive(Clone, Debug)]
pub struct BindInheritExpr {
    expr: Box<Expr>,
    names: Vec<Ident>,
    span: ByteSpan,
}

impl BindInheritExpr {
    pub fn new(expr: Box<Expr>, names: Vec<Ident>, span: ByteSpan) -> Self {
        BindInheritExpr { expr, names, span }
    }
}

impl Display for BindInheritExpr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let names: Vec<_> = self.names.iter().map(ToString::to_string).collect();
        write!(fmt, "inherit ({}) {};", self.expr, names.join(" "))
    }
}

impl PartialEq for BindInheritExpr {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr && self.names == other.names
    }
}

#[derive(Clone, Debug)]
pub struct ExprLet {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl ExprLet {
    pub fn new(binds: Vec<Bind>, span: ByteSpan) -> Self {
        ExprLet { binds, span }
    }
}

impl Display for ExprLet {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "let {{{}}}", binds.join(" "))
    }
}

impl PartialEq for ExprLet {
    fn eq(&self, other: &Self) -> bool {
        self.binds == other.binds
    }
}

#[derive(Clone, Debug)]
pub struct ExprRec {
    binds: Vec<Bind>,
    span: ByteSpan,
}

impl ExprRec {
    pub fn new(binds: Vec<Bind>, span: ByteSpan) -> Self {
        ExprRec { binds, span }
    }
}

impl Display for ExprRec {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "rec {{{}}}", binds.join(" "))
    }
}

impl PartialEq for ExprRec {
    fn eq(&self, other: &Self) -> bool {
        self.binds == other.binds
    }
}

#[derive(Clone, Debug)]
pub enum AttrKey {
    Ident(Ident),
    String(String, ByteSpan),
    Expr(Box<Expr>),
}

impl Display for AttrKey {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            AttrKey::Ident(ref i) => write!(fmt, "{}", i),
            AttrKey::String(ref s, _) => write!(fmt, "{}", s),
            AttrKey::Expr(ref e) => write!(fmt, "{}", e),
        }
    }
}

impl PartialEq for AttrKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AttrKey::Ident(ref i1), AttrKey::Ident(ref i2)) => i1 == i2,
            (AttrKey::String(ref s1, _), AttrKey::String(ref s2, _)) => s1 == s2,
            (AttrKey::Expr(ref e1), AttrKey::Expr(ref e2)) => e1 == e2,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprProj {
    expr: Box<Expr>,
    attr: AttrKey,
    fallback: Option<Box<Expr>>,
    span: ByteSpan,
}

impl ExprProj {
    pub fn new(
        expr: Box<Expr>,
        attr: AttrKey,
        fallback: Option<Box<Expr>>,
        span: ByteSpan,
    ) -> Self {
        ExprProj {
            expr,
            attr,
            fallback,
            span,
        }
    }
}

impl Display for ExprProj {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        if let Some(ref val) = self.fallback.as_ref() {
            write!(fmt, "{}.{} or {}", self.expr, self.attr, val)
        } else {
            write!(fmt, "{}.{}", self.expr, self.attr)
        }
    }
}

impl PartialEq for ExprProj {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr && self.attr == other.attr
    }
}

#[derive(Clone, Debug)]
pub struct ExprIf {
    cond: Box<Expr>,
    body: Box<Expr>,
    fallback: Box<Expr>,
    span: ByteSpan,
}

impl ExprIf {
    pub fn new(cond: Box<Expr>, body: Box<Expr>, fallback: Box<Expr>, span: ByteSpan) -> Self {
        ExprIf {
            cond,
            body,
            fallback,
            span,
        }
    }
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

impl PartialEq for ExprIf {
    fn eq(&self, other: &Self) -> bool {
        self.cond == other.cond && self.body == other.body && self.fallback == other.fallback
    }
}

#[derive(Clone, Debug)]
pub struct ExprOr {
    expr: Box<Expr>,
    fallback: Box<Expr>,
    span: ByteSpan,
}

impl ExprOr {
    pub fn new(expr: Box<Expr>, fallback: Box<Expr>, span: ByteSpan) -> Self {
        ExprOr {
            expr,
            fallback,
            span,
        }
    }
}

impl Display for ExprOr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} or {}", self.expr, self.fallback)
    }
}

impl PartialEq for ExprOr {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr && self.fallback == other.fallback
    }
}

#[derive(Clone, Debug)]
pub struct ExprAssert {
    cond: Box<Expr>,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl ExprAssert {
    pub fn new(cond: Box<Expr>, expr: Box<Expr>, span: ByteSpan) -> Self {
        ExprAssert { cond, expr, span }
    }
}

impl Display for ExprAssert {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "assert {}; {}", self.cond, self.expr)
    }
}

impl PartialEq for ExprAssert {
    fn eq(&self, other: &Self) -> bool {
        self.cond == other.cond && self.expr == other.expr
    }
}

#[derive(Clone, Debug)]
pub struct ExprWith {
    with: Box<Expr>,
    expr: Box<Expr>,
    span: ByteSpan,
}

impl ExprWith {
    pub fn new(with: Box<Expr>, expr: Box<Expr>, span: ByteSpan) -> Self {
        ExprWith { with, expr, span }
    }
}

impl Display for ExprWith {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "with {}; {}", self.with, self.expr)
    }
}

impl PartialEq for ExprWith {
    fn eq(&self, other: &Self) -> bool {
        self.with == other.with && self.expr == other.expr
    }
}

#[derive(Clone, Debug)]
pub struct ExprLetIn {
    binds: Vec<Bind>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl ExprLetIn {
    pub fn new(binds: Vec<Bind>, body: Box<Expr>, span: ByteSpan) -> Self {
        ExprLetIn { binds, body, span }
    }
}

impl Display for ExprLetIn {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let binds: Vec<_> = self.binds.iter().map(ToString::to_string).collect();
        write!(fmt, "let {} in {}", binds.join(" "), self.body)
    }
}

impl PartialEq for ExprLetIn {
    fn eq(&self, other: &Self) -> bool {
        self.binds == other.binds && self.body == other.body
    }
}

#[derive(Clone, Debug)]
pub enum Formal {
    Simple(Ident),
    Default(Ident, Box<Expr>, ByteSpan),
    Ellipsis(ByteSpan),
}

impl Display for Formal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Formal::Simple(ref name) => write!(fmt, "{}", name),
            Formal::Default(ref name, ref default, _) => write!(fmt, "{} ? {}", name, default),
            Formal::Ellipsis(_) => fmt.write_str("..."),
        }
    }
}

impl PartialEq for Formal {
    fn eq(&self, other: &Self) -> bool {
        use Formal::*;
        match (self, other) {
            (Simple(ref i1), Simple(ref i2)) => i1 == i2,
            (Default(ref n1, ref d1, _), Default(ref n2, ref d2, _)) => n1 == n2 && d1 == d2,
            (Ellipsis(_), Ellipsis(_)) => true,
            _ => false,
        }
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

#[derive(Clone, Debug)]
pub struct SimpleFnDecl {
    name: Ident,
    body: Box<Expr>,
    span: ByteSpan,
}

impl SimpleFnDecl {
    pub fn new(name: Ident, body: Box<Expr>, span: ByteSpan) -> Self {
        SimpleFnDecl { name, body, span }
    }
}

impl Display for SimpleFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}: {}", self.name, self.body)
    }
}

impl PartialEq for SimpleFnDecl {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.body == other.body
    }
}

#[derive(Clone, Debug)]
pub struct FormalsFnDecl {
    formals: Vec<Formal>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl FormalsFnDecl {
    pub fn new(formals: Vec<Formal>, body: Box<Expr>, span: ByteSpan) -> Self {
        FormalsFnDecl {
            formals,
            body,
            span,
        }
    }
}

impl Display for FormalsFnDecl {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let formals: Vec<_> = self.formals.iter().map(ToString::to_string).collect();
        write!(fmt, "{{{}}}: {}", formals.join(", "), self.body)
    }
}

impl PartialEq for FormalsFnDecl {
    fn eq(&self, other: &Self) -> bool {
        self.formals == other.formals && self.body == other.body
    }
}

#[derive(Clone, Debug)]
pub struct FormalsExtraFnDecl {
    extra: Ident,
    is_prefix: bool,
    formals: Vec<Formal>,
    body: Box<Expr>,
    span: ByteSpan,
}

impl FormalsExtraFnDecl {
    pub fn new(
        extra: Ident,
        is_prefix: bool,
        formals: Vec<Formal>,
        body: Box<Expr>,
        span: ByteSpan,
    ) -> Self {
        FormalsExtraFnDecl {
            extra,
            is_prefix,
            formals,
            body,
            span,
        }
    }
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

impl PartialEq for FormalsExtraFnDecl {
    fn eq(&self, other: &Self) -> bool {
        self.extra == other.extra
            && self.is_prefix == other.is_prefix
            && self.formals == other.formals
            && self.body == other.body
    }
}

#[derive(Clone, Debug)]
pub struct ExprFnApp {
    function: Box<Expr>,
    argument: Box<Expr>,
    span: ByteSpan,
}

impl ExprFnApp {
    pub fn new(function: Box<Expr>, argument: Box<Expr>, span: ByteSpan) -> Self {
        ExprFnApp {
            function,
            argument,
            span,
        }
    }
}

impl Display for ExprFnApp {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{} {}", self.function, self.argument)
    }
}

impl PartialEq for ExprFnApp {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.argument == other.argument
    }
}
