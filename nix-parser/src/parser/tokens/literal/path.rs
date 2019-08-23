use std::path::PathBuf;

use nom::branch::alt;
use nom::bytes::complete::{is_a, take_while1};
use nom::character::{complete::char, is_alphanumeric};
use nom::combinator::{cut, map, opt, recognize};
use nom::multi::separated_nonempty_list;
use nom::sequence::{delimited, tuple};

use crate::parser::{IResult, LocatedSpan};

pub fn path(input: LocatedSpan) -> IResult<PathBuf> {
    let prefix = opt(alt((is_a("~"), path_segment)));
    let segments = separated_nonempty_list(char('/'), path_segment);
    let path = recognize(tuple((prefix, char('/'), segments)));
    map(path, |p: LocatedSpan| PathBuf::from(p.fragment))(input)
}

fn path_segment(input: LocatedSpan) -> IResult<LocatedSpan> {
    take_while1(|c: char| is_alphanumeric(c as u8) || "._-+".contains(c))(input)
}

pub fn path_template(input: LocatedSpan) -> IResult<String> {
    let name = take_while1(|c: char| is_alphanumeric(c as u8) || "/._-+".contains(c));
    let template = delimited(char('<'), name, cut(char('>')));
    map(template, |tmp: LocatedSpan| tmp.fragment.to_string())(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    #[test]
    fn absolute_paths() {
        let string = LocatedSpan::new("/.");
        let (_, root) = all_consuming(path)(string).unwrap();
        assert_eq!(root.to_string_lossy(), "/.");

        let string = LocatedSpan::new("/foo/bar");
        let (_, simple_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_absolute.to_string_lossy(), "/foo/bar");

        let string = LocatedSpan::new("/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_absolute) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_absolute.to_string_lossy(), "/a/b.c/d+e/F_G/h-i/123");

        let string = LocatedSpan::new("~/foo/bar");
        let (_, simple_home) = all_consuming(path)(string).unwrap();
        assert_eq!(simple_home.to_string_lossy(), "~/foo/bar");

        let string = LocatedSpan::new("~/a/b.c/d+e/F_G/h-i/123");
        let (_, complex_home) = all_consuming(path)(string).unwrap();
        assert_eq!(complex_home.to_string_lossy(), "~/a/b.c/d+e/F_G/h-i/123");
    }

    #[test]
    fn relative_paths() {
        let string = LocatedSpan::new("./.");
        let (_, current_dir) = all_consuming(path)(string).unwrap();
        assert_eq!(current_dir.to_string_lossy(), "./.");

        let string = LocatedSpan::new("../.");
        let (_, parent_dir) = all_consuming(path)(string).unwrap();
        assert_eq!(parent_dir.to_string_lossy(), "../.");

        let string = LocatedSpan::new("./foo/bar");
        let (_, simple) = all_consuming(path)(string).unwrap();
        assert_eq!(simple.to_string_lossy(), "./foo/bar");

        let string = LocatedSpan::new("./a/b.c/d+e/F_G/h-i/123");
        let (_, complex) = all_consuming(path)(string).unwrap();
        assert_eq!(complex.to_string_lossy(), "./a/b.c/d+e/F_G/h-i/123");

        let string = LocatedSpan::new("foo/bar");
        let (_, no_prefix) = all_consuming(path)(string).unwrap();
        assert_eq!(no_prefix.to_string_lossy(), "foo/bar");
    }

    #[test]
    fn path_templates() {
        let string = LocatedSpan::new("<nixpkgs>");
        let (_, nixpkgs) = all_consuming(path_template)(string).unwrap();
        assert_eq!(nixpkgs, "nixpkgs");

        let string = LocatedSpan::new("<Foo.bar-baz_quux+123>");
        let (_, complex) = all_consuming(path_template)(string).unwrap();
        assert_eq!(complex, "Foo.bar-baz_quux+123");
    }
}
