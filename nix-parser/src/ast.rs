pub use self::tokens::{Ident, Literal};

use std::fmt::{Display, Formatter, Result as FmtResult};

use codespan::Span;

mod tokens;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// A parenthesized expression.
    ///
    /// This type of expression is only used to aid with serialization of the AST back into a token
    /// string.
    Paren(Box<Expr>),
    /// `12`, `4.0`, `false`, `"foo"`, `''bar''`, `./foo/bar`
    Literal(Literal),

    /// `-12`
    /// `!15.0`
    Unary(UnaryOp, Box<Expr>),
    /// `1 + 1`, `true && false`
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Expr::Paren(ref expr) => write!(fmt, "({})", expr),
            Expr::Literal(ref lit) => write!(fmt, "{}", lit),

            Expr::Unary(ref op, ref expr) => write!(fmt, "{}{}", op, expr),
            Expr::Binary(ref op, ref lhs, ref rhs) => write!(fmt, "{} {} {}", lhs, op, rhs),
        }
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
