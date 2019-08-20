use nom::branch::alt;
use nom::character::complete::{anychar, char, multispace0};
use nom::combinator::{cut, map, opt, recognize};
use nom::error::ErrorKind;
use nom::multi::{many1, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};

use super::expr;
use crate::ast::{Bind, BindInherit, BindInheritExpr, BindSimple};
use crate::parser::partial::{expect_terminated, map_err_partial, map_partial, Partial};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn bind(input: Span) -> IResult<Partial<Bind>> {
    let inherit = map_partial(inherit, Bind::Inherit);
    let inherit_expr = map_partial(inherit_expr, Bind::InheritExpr);
    let simple = map_partial(simple, Bind::Simple);
    let bind = expect_terminated(alt((inherit, inherit_expr, simple)), char(';'));
    map_err_partial(bind, char(';'), ErrorKind::IsA)(input)
}

fn simple(input: Span) -> IResult<Partial<BindSimple>> {
    let comment = opt(terminated(tokens::comment, multispace0));

    let lhs = map(terminated(tokens::ident_path, tokens::space), Partial::from);
    let rhs = map_partial(preceded(tokens::space, expr), Box::new);
    let bind = tuple((comment, expect_terminated(lhs, char('=')), rhs));

    map(bind, |(comment, name, expr)| {
        name.flat_map(|name| {
            expr.map(|expr| BindSimple::new(comment, name, expr, input.to_byte_span()))
        })
    })(input)
}

fn inherit(input: Span) -> IResult<Partial<BindInherit>> {
    let comment = opt(terminated(tokens::comment, multispace0));
    let key_inherit = pair(tokens::keyword_inherit, tokens::space);
    let name = terminated(tokens::identifier, tokens::space);
    let bind = preceded(comment, preceded(key_inherit, many1(name)));
    map(bind, |names| {
        Partial::from(BindInherit::new(names, input.to_byte_span()))
    })(input)
}

fn inherit_expr(input: Span) -> IResult<Partial<BindInheritExpr>> {
    let comment = opt(terminated(tokens::comment, multispace0));
    let key_inherit = pair(tokens::keyword_inherit, tokens::space);

    let open_paren = pair(char('('), tokens::space);
    let close_paren = pair(char(')'), tokens::space);
    let expr = expect_terminated(preceded(open_paren, expr), close_paren);

    let name = terminated(tokens::identifier, tokens::space);
    let bind = preceded(key_inherit, pair(map_partial(expr, Box::new), many1(name)));
    map(bind, |(expr, names)| {
        expr.map(|expr| BindInheritExpr::new(expr, names, input.to_byte_span()))
    })(input)
}

#[cfg(test)]
mod tests {
    use nom::combinator::all_consuming;

    use super::*;
    use crate::ast::tokens::{Comment, Ident, IdentPath, Literal};
    use crate::ast::Expr;
    use crate::ToByteSpan;

    #[test]
    fn simple_binds() {
        let string = Span::new("foo.bar = true;");
        let (_, uncommented) = all_consuming(bind)(string).unwrap();
        assert_eq!(
            uncommented,
            Partial::from(Bind::Simple(BindSimple::new(
                None,
                IdentPath::from(vec!["foo", "bar"]),
                Box::new(Expr::Literal(Literal::from(true))),
                Span::new("").to_byte_span(),
            )))
        );

        let string = Span::new("# hello world \n #this is a   doc comment   \n  foo.bar = true;");
        let (_, commented) = all_consuming(bind)(string).unwrap();
        assert_eq!(
            commented,
            Partial::from(Bind::Simple(BindSimple::new(
                Some(Comment::from(" hello world \nthis is a   doc comment   ")),
                IdentPath::from(vec!["foo", "bar"]),
                Box::new(Expr::Literal(Literal::from(true))),
                Span::new("").to_byte_span(),
            )))
        );
    }
}
