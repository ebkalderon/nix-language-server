use nom::branch::alt;
use nom::character::complete::{char, multispace0};
use nom::combinator::{map, opt};
use nom::error::ErrorKind;
use nom::multi::many1;
use nom::sequence::{pair, preceded, terminated, tuple};

use super::expr;
use crate::ast::{Bind, BindInherit, BindInheritExpr, BindSimple};
use crate::parser::partial::{expect_terminated, map_err_partial, map_partial, Partial};
use crate::parser::{map_spanned, tokens, IResult, Span};

pub fn bind(input: Span) -> IResult<Partial<Bind>> {
    let inherit = map_partial(inherit, Bind::Inherit);
    let inherit_expr = map_partial(inherit_expr, Bind::InheritExpr);
    let simple = map_partial(simple, Bind::Simple);
    let bind = expect_terminated(alt((inherit, inherit_expr, simple)), char(';'));
    map_err_partial(bind, char(';'), ErrorKind::IsA)(input)
}

fn simple(input: Span) -> IResult<Partial<BindSimple>> {
    let (input, comment) = opt(terminated(tokens::comment, multispace0))(input)?;

    let lhs = map(terminated(tokens::ident_path, tokens::space), Partial::from);
    let rhs = map_partial(preceded(tokens::space, expr), Box::new);
    let simple = pair(expect_terminated(lhs, char('=')), rhs);
    let bind = map(simple, |(name, expr)| {
        name.flat_map(|name| expr.map(|expr| (name, expr)))
    });

    let (remainder, (span, bind)) = map_spanned(bind, |span, bind| (span, bind))(input)?;
    let simple = bind.map(|(name, expr)| BindSimple::new(comment, name, expr, span));

    Ok((remainder, simple))
}

fn inherit(input: Span) -> IResult<Partial<BindInherit>> {
    let comment = opt(terminated(tokens::comment, multispace0));
    let key_inherit = pair(tokens::keyword_inherit, tokens::space);
    let name = terminated(tokens::identifier, tokens::space);
    let bind = preceded(comment, preceded(key_inherit, many1(name)));
    map_spanned(bind, |span, names| {
        Partial::from(BindInherit::new(names, span))
    })(input)
}

fn inherit_expr(input: Span) -> IResult<Partial<BindInheritExpr>> {
    let comment = opt(terminated(tokens::comment, multispace0));
    let key_inherit = tuple((comment, tokens::keyword_inherit, tokens::space));

    let open_paren = pair(char('('), tokens::space);
    let close_paren = pair(char(')'), tokens::space);
    let expr = expect_terminated(preceded(open_paren, expr), close_paren);

    let name = terminated(tokens::identifier, tokens::space);
    let bind = preceded(key_inherit, pair(map_partial(expr, Box::new), many1(name)));
    map_spanned(bind, |span, (expr, names)| {
        expr.map(|expr| BindInheritExpr::new(expr, names, span))
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;
    use crate::ast::tokens::{Comment, Ident, IdentPath, Literal};
    use crate::ast::Expr;
    use crate::{nix, nix_bind, nix_token, ToByteSpan};

    #[test]
    fn simple() {
        let string = Span::new("foo.bar = true;");
        let (_, uncommented) = all_consuming(bind)(string).unwrap();
        assert_eq!(uncommented, Partial::from(nix_bind!(foo.bar = true)));

        let string = Span::new("# hello world \n #this is a   doc comment   \n  foo.bar = true;");
        let (_, commented) = all_consuming(bind)(string).unwrap();
        assert_eq!(
            commented,
            Partial::from(Bind::Simple(BindSimple::new(
                Some(Comment::from(" hello world \nthis is a   doc comment   ")),
                nix_token!(foo.bar),
                Box::new(nix!(true)),
                Span::new("").to_byte_span(),
            )))
        );
    }

    #[test]
    fn inherit() {
        let string = Span::new("inherit foo bar;");
        let (_, without_expr) = all_consuming(bind)(string).unwrap();
        assert_eq!(without_expr, Partial::from(nix_bind!(inherit foo bar)));

        let string = Span::new("inherit (42) foo bar;");
        let (_, with_expr) = all_consuming(bind)(string).unwrap();
        assert_eq!(with_expr, Partial::from(nix_bind!(inherit (42) foo bar)));
    }
}
