use std::cmp::Ordering;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};

use codespan::Span;
use http::Uri;

use crate::ToSpan;

#[derive(Clone, Debug, Eq)]
pub struct Comment(String, Span);

impl Display for Comment {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let lines: String = self.0.lines().map(|l| format!("#{}\n", l)).collect();
        write!(fmt, "{}", lines)
    }
}

impl<'a> From<&'a str> for Comment {
    fn from(s: &'a str) -> Self {
        Comment(s.to_owned(), Span::initial())
    }
}

impl From<String> for Comment {
    fn from(s: String) -> Self {
        Comment(s, Span::initial())
    }
}

impl<T, S> From<(T, S)> for Comment
where
    T: Into<String>,
    S: ToSpan,
{
    fn from((string, span): (T, S)) -> Self {
        Comment(string.into(), span.to_span())
    }
}

impl PartialEq for Comment {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for Comment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

#[derive(Clone, Debug, Eq)]
pub struct Ident(String, Span);

impl Display for Ident {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Ident(s.to_owned(), Span::initial())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s, Span::initial())
    }
}

impl<T, S> From<(T, S)> for Ident
where
    T: Into<String>,
    S: ToSpan,
{
    fn from((string, span): (T, S)) -> Self {
        Ident(string.into(), span.to_span())
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
pub struct IdentPath(Vec<Ident>, Span);

impl Display for IdentPath {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let idents: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(fmt, "{}", idents.join("."))
    }
}

impl<'a, T> From<Vec<T>> for IdentPath
where
    T: Into<Ident>,
{
    fn from(idents: Vec<T>) -> Self {
        let elems = idents.into_iter().map(Into::into).collect();
        IdentPath(elems, Span::initial())
    }
}

impl<'a, T, U, S> From<(U, S)> for IdentPath
where
    T: Into<Ident>,
    U: IntoIterator<Item = T>,
    S: ToSpan,
{
    fn from((idents, span): (U, S)) -> Self {
        let elems = idents.into_iter().map(Into::into).collect();
        IdentPath(elems, span.to_span())
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
    Null(Span),
    Boolean(bool, Span),
    Float(f64, Span),
    Integer(i64, Span),
    Path(PathBuf, Span),
    PathTemplate(String, Span),
    String(String, Span),
    Uri(Uri, Span),
}

impl Literal {
    pub fn path_template<T, U>(string: T, span: U) -> Self
    where
        T: Into<String>,
        U: ToSpan,
    {
        Literal::PathTemplate(string.into(), span.to_span())
    }
}

impl Display for Literal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Literal::Null(_) => write!(fmt, "null"),
            Literal::Boolean(ref b, _) => write!(fmt, "{}", b),
            Literal::Float(ref f, _) => write!(fmt, "{:?}", f),
            Literal::Integer(ref i, _) => write!(fmt, "{}", i),
            Literal::Path(ref p, _) => write!(fmt, "{}", p.to_string_lossy()),
            Literal::PathTemplate(ref p, _) => write!(fmt, "<{}>", p),
            Literal::String(ref s, _) => write!(fmt, "\"{}\"", s),
            Literal::Uri(ref u, _) => write!(fmt, "{}", u),
        }
    }
}

impl<T: Into<Literal>> From<Option<T>> for Literal {
    fn from(value: Option<T>) -> Self {
        value
            .map(Into::into)
            .unwrap_or(Literal::Null(Span::initial()))
    }
}

impl From<()> for Literal {
    fn from(_: ()) -> Self {
        Literal::Null(Span::initial())
    }
}

impl From<bool> for Literal {
    fn from(boolean: bool) -> Self {
        Literal::Boolean(boolean, Span::initial())
    }
}

impl From<f64> for Literal {
    fn from(float: f64) -> Self {
        Literal::Float(float, Span::initial())
    }
}

impl From<i64> for Literal {
    fn from(int: i64) -> Self {
        Literal::Integer(int, Span::initial())
    }
}

impl<'a> From<&'a Path> for Literal {
    fn from(path: &'a Path) -> Self {
        Literal::Path(path.to_owned(), Span::initial())
    }
}

impl From<PathBuf> for Literal {
    fn from(path: PathBuf) -> Self {
        Literal::Path(path, Span::initial())
    }
}

impl<'a> From<&'a str> for Literal {
    fn from(s: &'a str) -> Self {
        Literal::String(s.to_owned(), Span::initial())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s, Span::initial())
    }
}

impl<S: ToSpan> From<((), S)> for Literal {
    fn from((_, span): ((), S)) -> Self {
        Literal::Null(span.to_span())
    }
}

impl<S: ToSpan> From<(bool, S)> for Literal {
    fn from((boolean, span): (bool, S)) -> Self {
        Literal::Boolean(boolean, span.to_span())
    }
}

impl<S: ToSpan> From<(f64, S)> for Literal {
    fn from((float, span): (f64, S)) -> Self {
        Literal::Float(float, span.to_span())
    }
}

impl<S: ToSpan> From<(i64, S)> for Literal {
    fn from((int, span): (i64, S)) -> Self {
        Literal::Integer(int, span.to_span())
    }
}

impl<'a, S: ToSpan> From<(&'a Path, S)> for Literal {
    fn from((path, span): (&'a Path, S)) -> Self {
        Literal::Path(path.to_owned(), span.to_span())
    }
}

impl<S: ToSpan> From<(PathBuf, S)> for Literal {
    fn from((path, span): (PathBuf, S)) -> Self {
        Literal::Path(path, span.to_span())
    }
}

impl<'a, S: ToSpan> From<(&'a str, S)> for Literal {
    fn from((s, span): (&'a str, S)) -> Self {
        Literal::String(s.to_owned(), span.to_span())
    }
}

impl<S: ToSpan> From<(String, S)> for Literal {
    fn from((s, span): (String, S)) -> Self {
        Literal::String(s, span.to_span())
    }
}

impl<S: ToSpan> From<(Uri, S)> for Literal {
    fn from((uri, span): (Uri, S)) -> Self {
        Literal::Uri(uri, span.to_span())
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
