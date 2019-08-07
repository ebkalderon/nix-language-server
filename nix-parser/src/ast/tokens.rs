use std::fmt::{Display, Formatter, Result as FmtResult};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

use codespan::ByteSpan;
use http::Uri;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Ident(String, ByteSpan);

impl<'a> From<(&'a str, ByteSpan)> for Ident {
    fn from((string, span): (&'a str, ByteSpan)) -> Self {
        Ident(string.to_owned(), span)
    }
}

impl From<(String, ByteSpan)> for Ident {
    fn from((string, span): (String, ByteSpan)) -> Self {
        Ident(string, span)
    }
}

impl Display for Ident {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct IdentPath(Vec<Ident>, ByteSpan);

impl<'a, T, U> From<(U, ByteSpan)> for IdentPath
where
    T: Into<Ident>,
    U: IntoIterator<Item = T>,
{
    fn from((idents, span): (U, ByteSpan)) -> Self {
        IdentPath(idents.into_iter().map(Into::into).collect(), span)
    }
}

impl Display for IdentPath {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let idents: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(fmt, "{}", idents.join("."))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Null(ByteSpan),
    Boolean(bool, ByteSpan),
    Float(f64, ByteSpan),
    Integer(i64, ByteSpan),
    Path(PathBuf, ByteSpan),
    PathTemplate(String, ByteSpan),
    String(String, ByteSpan),
    Uri(Uri, ByteSpan),
}

impl From<((), ByteSpan)> for Literal {
    fn from((_, span): ((), ByteSpan)) -> Self {
        Literal::Null(span)
    }
}

impl From<(bool, ByteSpan)> for Literal {
    fn from((boolean, span): (bool, ByteSpan)) -> Self {
        Literal::Boolean(boolean, span)
    }
}

impl From<(f64, ByteSpan)> for Literal {
    fn from((float, span): (f64, ByteSpan)) -> Self {
        Literal::Float(float, span)
    }
}

impl From<(i64, ByteSpan)> for Literal {
    fn from((int, span): (i64, ByteSpan)) -> Self {
        Literal::Integer(int, span)
    }
}

impl<'a> From<(&'a Path, ByteSpan)> for Literal {
    fn from((path, span): (&'a Path, ByteSpan)) -> Self {
        Literal::Path(path.to_owned(), span)
    }
}

impl From<(PathBuf, ByteSpan)> for Literal {
    fn from((path, span): (PathBuf, ByteSpan)) -> Self {
        Literal::Path(path, span)
    }
}

impl<'a> From<(&'a str, ByteSpan)> for Literal {
    fn from((s, span): (&'a str, ByteSpan)) -> Self {
        Literal::String(s.to_owned(), span)
    }
}

impl From<(String, ByteSpan)> for Literal {
    fn from((s, span): (String, ByteSpan)) -> Self {
        Literal::String(s, span)
    }
}

impl From<(Uri, ByteSpan)> for Literal {
    fn from((uri, span): (Uri, ByteSpan)) -> Self {
        Literal::Uri(uri, span)
    }
}

impl Display for Literal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Literal::Null(_) => write!(fmt, "null"),
            Literal::Boolean(ref b, _) => write!(fmt, "{}", b),
            Literal::Float(ref f, _) => write!(fmt, "{}", f),
            Literal::Integer(ref i, _) => write!(fmt, "{}", i),
            Literal::Path(ref p, _) => write!(fmt, "{}", p.to_string_lossy()),
            Literal::PathTemplate(ref p, _) => write!(fmt, "<{}>", p),
            Literal::String(ref s, _) => write!(fmt, "\"{}\"", s),
            Literal::Uri(ref u, _) => write!(fmt, "{}", u),
        }
    }
}
