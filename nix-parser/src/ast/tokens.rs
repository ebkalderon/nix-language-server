use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};

use codespan::ByteSpan;
use http::Uri;

#[derive(Clone, Debug, Eq)]
pub struct Ident(String, ByteSpan);

impl Display for Ident {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

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

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

#[derive(Clone, Debug, Eq)]
pub struct IdentPath(Vec<Ident>, ByteSpan);

impl Display for IdentPath {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let idents: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(fmt, "{}", idents.join("."))
    }
}

impl<'a, T, U> From<(U, ByteSpan)> for IdentPath
where
    T: Into<Ident>,
    U: IntoIterator<Item = T>,
{
    fn from((idents, span): (U, ByteSpan)) -> Self {
        IdentPath(idents.into_iter().map(Into::into).collect(), span)
    }
}

impl PartialEq for IdentPath {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for IdentPath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

#[derive(Clone, Debug)]
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

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        use Literal::*;
        match (self, other) {
            (Null(_), Null(_)) => true,
            (Boolean(ref b1, _), Boolean(ref b2, _)) => *b1 == *b2,
            (Float(ref f1, _), Float(ref f2, _)) => f1 == f2,
            (Integer(ref i1, _), Integer(ref i2, _)) => i1 == i2,
            (Path(ref p1, _), Path(ref p2, _)) => p1 == p2,
            (PathTemplate(ref p1, _), PathTemplate(ref p2, _)) => p1 == p2,
            (String(ref s1, _), String(ref s2, _)) => s1 == s2,
            (Uri(ref u1, _), Uri(ref u2, _)) => u1 == u2,
            _ => false,
        }
    }
}

impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Literal::*;
        match (self, other) {
            (Null(_), Null(_)) => Some(Ordering::Equal),
            (Boolean(ref b1, _), Boolean(ref b2, _)) => b1.partial_cmp(b2),
            (Float(ref f1, _), Float(ref f2, _)) => f1.partial_cmp(f2),
            (Integer(ref i1, _), Integer(ref i2, _)) => i1.partial_cmp(i2),
            (Path(ref p1, _), Path(ref p2, _)) => p1.partial_cmp(p2),
            (PathTemplate(ref p1, _), PathTemplate(ref p2, _)) => p1.partial_cmp(p2),
            (String(ref s1, _), String(ref s2, _)) => s1.partial_cmp(s2),
            (Uri(ref u1, _), Uri(ref u2, _)) => u1.to_string().partial_cmp(&u2.to_string()),
            _ => None,
        }
    }
}
