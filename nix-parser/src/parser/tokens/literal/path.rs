use std::path::PathBuf;

use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::character::complete::{alphanumeric1, char};
use nom::combinator::{map, opt, recognize};
use nom::multi::{count, many1, separated_nonempty_list};
use nom::sequence::{delimited, pair};

use crate::parser::{IResult, Spanned};

pub fn path(input: Spanned) -> IResult<PathBuf> {
    let prefix = pair(opt(count(is_a("~."), 1)), char('/'));
    let segment = many1(alt((alphanumeric1, is_a("._-+"))));
    let path = recognize(pair(prefix, separated_nonempty_list(char('/'), segment)));
    map(path, |p: Spanned| PathBuf::from(p.fragment))(input)
}

pub fn path_template(input: Spanned) -> IResult<String> {
    let name = recognize(many1(alt((alphanumeric1, is_a("._-+")))));
    let template = delimited(char('<'), name, char('>'));
    map(template, |tmp: Spanned| tmp.fragment.to_string())(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn absolute_paths() {
        let string = Spanned::new("/.");
        let (_, root) = all_consuming(path)(string).unwrap();
        assert_eq!(root.to_string_lossy(), "/.");

        let string = Spanned::new("/foo/bar");
        let (_, simple_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_absolute.to_string_lossy(), "/foo/bar");

        let string = Spanned::new("/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_absolute.to_string_lossy(), "/a/b.c/d+e/F_G/h-i/123");

        let string = Spanned::new("~/foo/bar");
        let (_, simple_home) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_home.to_string_lossy(), "~/foo/bar");

        let string = Spanned::new("~/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_home) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_home.to_string_lossy(), "~/a/b.c/d+e/F_G/h-i/123");
    }

    #[test]
    fn relative_paths() {
        let string = Spanned::new("./.");
        let (_, current_dir) = all_consuming(path)(string).unwrap();
        assert_eq!(current_dir.to_string_lossy(), "./.");

        let string = Spanned::new("./foo/bar");
        let (_, simple) = all_consuming(path)(string).unwrap();
        assert_eq!(simple.to_string_lossy(), "./foo/bar");

        let string = Spanned::new("./a/b.c/d+e/F_G/h-i/123");
        let (_, complex) = all_consuming(path)(string).unwrap();
        assert_eq!(complex.to_string_lossy(), "./a/b.c/d+e/F_G/h-i/123");
    }

    #[test]
    fn path_templates() {
        let string = Spanned::new("<nixpkgs>");
        let (_, nixpkgs) = all_consuming(path_template)(string).unwrap();
        assert_eq!(nixpkgs, "nixpkgs");

        let string = Spanned::new("<Foo.bar-baz_quux+123>");
        let (_, complex) = all_consuming(path_template)(string).unwrap();
        assert_eq!(complex, "Foo.bar-baz_quux+123");
    }
}
