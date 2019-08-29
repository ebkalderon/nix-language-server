use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::sequence::{pair, terminated};

use super::partial::{map_partial, Partial};
use super::{map_spanned, tokens, IResult, LocatedSpan};
use crate::ast::{Expr, ExprUnary, UnaryOp};

mod app;
mod atomic;
mod bind;
mod membership;

pub fn expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    terminated(unary, tokens::space)(input)
}

fn unary(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let neg = map(char('-'), |_| UnaryOp::Neg);
    let not = map(char('!'), |_| UnaryOp::Not);
    let expr = pair(opt(alt((neg, not))), membership::membership_expr);
    map_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => expr.map(|expr| Expr::Unary(ExprUnary::new(op, Box::new(expr), span))),
        None => expr,
    })(input)
}

fn unary_in_list(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let neg = map(char('-'), |_| UnaryOp::Neg);
    let not = map(char('!'), |_| UnaryOp::Not);
    let expr = pair(opt(alt((neg, not))), membership::membership_in_list_expr);
    map_spanned(expr, |span, (unary, expr)| match unary {
        Some(op) => expr.map(|expr| Expr::Unary(ExprUnary::new(op, Box::new(expr), span))),
        None => expr,
    })(input)
}

pub fn atomic(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let paren = map_partial(atomic::paren, Expr::Paren);
    let set = map_partial(atomic::set, Expr::Set);
    let list = map_partial(atomic::list, Expr::List);
    let literal = map(map(tokens::literal, Expr::Literal), Partial::from);
    let attr = map(map(tokens::ident_path, Expr::Attr), Partial::from);
    alt((paren, set, list, literal, attr, tokens::string))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::combinator::all_consuming;

    use crate::ast::{BinaryOp, ExprBinary, ExprList};

    #[test]
    fn list() {
        let input = LocatedSpan::new("[ a b c ? d ]");
        let (_, val) = all_consuming(atomic)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::List(ExprList::new(
                vec![
                    Expr::Attr(vec!["a"].into()),
                    Expr::Attr(vec!["b"].into()),
                    Expr::Binary(ExprBinary::new(
                        BinaryOp::HasAttr,
                        Box::new(Expr::Attr(vec!["c"].into()),),
                        Box::new(Expr::Attr(vec!["d"].into()),),
                        Default::default()
                    )),
                ],
                Default::default()
            )))
        );
    }
}
