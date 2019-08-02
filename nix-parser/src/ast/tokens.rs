use std::fmt::{Display, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};

use http::Uri;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Ident(String);

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Ident(s.to_owned())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s)
    }
}

impl Display for Ident {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Null,
    Boolean(bool),
    Float(f64),
    Integer(i64),
    Path(PathBuf),
    String(String),
    Uri(Uri),
}

impl<T: Into<Literal>> From<Option<T>> for Literal {
    fn from(value: Option<T>) -> Self {
        value.map(|lit| lit.into()).unwrap_or_else(|| Literal::Null)
    }
}

impl From<bool> for Literal {
    fn from(boolean: bool) -> Self {
        Literal::Boolean(boolean)
    }
}

impl From<f64> for Literal {
    fn from(float: f64) -> Self {
        Literal::Float(float)
    }
}

impl From<i64> for Literal {
    fn from(int: i64) -> Self {
        Literal::Integer(int)
    }
}

impl<'a> From<&'a Path> for Literal {
    fn from(p: &'a Path) -> Self {
        Literal::Path(p.to_owned())
    }
}

impl From<PathBuf> for Literal {
    fn from(p: PathBuf) -> Self {
        Literal::Path(p)
    }
}

impl<'a> From<&'a str> for Literal {
    fn from(s: &'a str) -> Self {
        Literal::String(s.to_owned())
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}

impl From<Uri> for Literal {
    fn from(uri: Uri) -> Self {
        Literal::Uri(uri)
    }
}

impl Display for Literal {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Literal::Null => write!(fmt, "null"),
            Literal::Boolean(ref b) => write!(fmt, "{}", b),
            Literal::Float(ref f) => write!(fmt, "{}", f),
            Literal::Integer(ref i) => write!(fmt, "{}", i),
            Literal::Path(ref p) => write!(fmt, "{}", p.to_string_lossy()),
            Literal::String(ref s) => write!(fmt, "\"{}\"", s),
            Literal::Uri(ref u) => write!(fmt, "{}", u),
        }
    }
}
