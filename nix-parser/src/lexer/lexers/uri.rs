//! Combinator for lexing URIs.

use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{anychar, char};
use nom::combinator::{recognize, verify};
use nom::sequence::{pair, tuple};

use crate::lexer::util::map_spanned;
use crate::lexer::{IResult, LocatedSpan, Token};

pub fn uri(input: LocatedSpan) -> IResult<Token> {
    let first = verify(anychar, |c| c.is_alphabetic());
    let rest = take_while(|c: char| c.is_alphanumeric() || "+-.".contains(c));
    let scheme = pair(first, rest);

    let path = take_while1(|c: char| c.is_alphanumeric() || "%/?:@&=+$,-_.!~*'".contains(c));
    let uri = recognize(tuple((scheme, char(':'), path)));

    map_spanned(uri, |span, uri| Token::Uri(uri.fragment.into(), span))(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;

    fn assert_uri_eq(string: &str) {
        let span = LocatedSpan::new(string);
        match all_consuming(uri)(span) {
            Ok((_, Token::Uri(value, _))) => assert_eq!(value, string),
            Ok((_, token)) => panic!("parsing {:?} produced token: {:?}", string, token),
            Err(err) => panic!("parsing {:?} failed: {:?}", string, err),
        }
    }

    #[test]
    fn short_uri() {
        assert_uri_eq("x:x");
    }

    #[test]
    fn with_explicit_port() {
        assert_uri_eq("https://svn.cs.uu.nl:12443/repos/trace/trunk");
    }

    #[test]
    fn font_pack_uri() {
        assert_uri_eq(
            "http://www2.mplayerhq.hu/MPlayer/releases/fonts/font-arial-iso-8859-1.tar.bz2",
        );
    }

    #[test]
    fn academic_uri() {
        assert_uri_eq("http://losser.st-lab.cs.uu.nl/~armijn/.nix/gcc-3.3.4-static-nix.tar.gz");
    }

    #[test]
    fn complex_uri() {
        assert_uri_eq("http://fpdownload.macromedia.com/get/shockwave/flash/english/linux/7.0r25/install_flash_player_7_linux.tar.gz");
    }

    #[test]
    fn ftp_uri() {
        assert_uri_eq("ftp://ftp.gtk.org/pub/gtk/v1.2/gtk+-1.2.10.tar.gz");
    }
}
