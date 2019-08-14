use nom::branch::alt;
use nom::bytes::complete::take_until;
use nom::character::complete::{char, multispace0};
use nom::combinator::{all_consuming, map, map_parser, opt};
use nom::multi::many1;
use nom::sequence::{delimited, pair, separated_pair, terminated};

use super::expr;
use crate::ast::{Bind, BindInherit, BindInheritExpr, BindSimple};
use crate::parser::{tokens, IResult, Span};
use crate::ToByteSpan;

pub fn bind(input: Span) -> IResult<Bind> {
    let inherit = map(inherit, Bind::Inherit);
    let inherit_expr = map(inherit_expr, Bind::InheritExpr);
    let simple = map(simple, Bind::Simple);
    alt((inherit, inherit_expr, simple))(input)
}

fn simple(input: Span) -> IResult<BindSimple> {
    let comment = opt(terminated(tokens::comment, multispace0));

    let name = terminated(tokens::ident_path, tokens::space);
    let expr = map(expr, Box::new);

    let lhs = map_parser(take_until("="), all_consuming(name));
    let equals = pair(char('='), tokens::space);
    let rhs = map_parser(take_until(";"), all_consuming(expr));
    let semi = pair(tokens::space, char(';'));
    let bind = pair(comment, terminated(separated_pair(lhs, equals, rhs), semi));

    map(bind, |(comment, (name, expr))| {
        BindSimple::new(comment, name, expr, input.to_byte_span())
    })(input)
}

fn inherit(input: Span) -> IResult<BindInherit> {
    let key_inherit = pair(tokens::keyword_inherit, tokens::space);
    let name = terminated(tokens::identifier, tokens::space);
    let inherit = delimited(key_inherit, many1(name), char(';'));
    map(inherit, |names| {
        BindInherit::new(names, input.to_byte_span())
    })(input)
}

fn inherit_expr(input: Span) -> IResult<BindInheritExpr> {
    let key_inherit = pair(tokens::keyword_inherit, tokens::space);

    let open_paren = pair(char('('), tokens::space);
    let close_paren = pair(char(')'), tokens::space);
    let expr = map(delimited(open_paren, expr, close_paren), Box::new);

    let name = terminated(tokens::identifier, tokens::space);
    let inherit = delimited(key_inherit, pair(expr, many1(name)), char(';'));

    map(inherit, |(expr, names)| {
        BindInheritExpr::new(expr, names, input.to_byte_span())
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::tokens::{Comment, Ident, IdentPath, Literal};
    use crate::ast::Expr;
    use crate::ToByteSpan;

    #[test]
    fn simple_binds() {
        let string = Span::new("foo.bar = true;");
        let (_, uncommented) = all_consuming(simple)(string).unwrap();
        assert_eq!(
            uncommented,
            BindSimple::new(
                None,
                IdentPath::from(vec!["foo", "bar"]),
                Box::new(Expr::Literal(Literal::from(true))),
                Span::new("").to_byte_span(),
            )
        );

        let string = Span::new("# hello world \n #this is a   doc comment   \n  foo.bar = true;");
        let (_, commented) = all_consuming(simple)(string).unwrap();
        assert_eq!(
            commented,
            BindSimple::new(
                Some(Comment::from("hello world \nthis is a   doc comment   ")),
                IdentPath::from(vec!["foo", "bar"]),
                Box::new(Expr::Literal(Literal::from(true))),
                Span::new("").to_byte_span(),
            )
        );
    }
}
