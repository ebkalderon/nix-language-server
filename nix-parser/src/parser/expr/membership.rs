use nom::{bytes::complete::tag, character::complete::char, combinator::map, sequence::preceded};

use super::{app::app_expr, atomic, expr};
use crate::{
    ast::{BinaryOp, Expr, ExprBinary},
    parser::{
        error::{Error, Errors},
        partial::{expect_terminated, Partial},
        tokens::{ident_path, space, string},
        IResult, LocatedSpan,
    },
};

pub fn membership_expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let (input, expr) = app_expr(input)?;
    if let Ok((input, _)) = preceded(preceded(space, char('?')), space)(input) {
        membership_test_expr(expr, input)
    } else {
        Ok((input, expr))
    }
}

pub fn membership_in_list_expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    let (input, expr) = atomic(input)?;
    if let Ok((input, _)) = preceded(preceded(space, char('?')), space)(input) {
        membership_test_expr(expr, input)
    } else {
        Ok((input, expr))
    }
}

fn membership_test_expr(set: Partial<Expr>, input: LocatedSpan) -> IResult<Partial<Expr>> {
    use crate::HasSpan;
    if let Ok((input, ident)) = map(ident_path, Expr::Attr)(input) {
        let ident_span = ident.span();
        Ok((
            input,
            set.map(|set| {
                Expr::Binary(ExprBinary::new(
                    BinaryOp::HasAttr,
                    Box::new(set),
                    Box::new(ident),
                    ident_span,
                ))
            }),
        ))
    } else if let Ok((input, ident)) =
        preceded(tag("${"), expect_terminated(expr, char('}')))(input)
    {
        Ok((
            input,
            ident.flat_map(move |ident| {
                let span = ident.span();
                set.map(|set| {
                    Expr::Binary(ExprBinary::new(
                        BinaryOp::HasAttr,
                        Box::new(set),
                        Box::new(ident),
                        span,
                    ))
                })
            }),
        ))
    } else if let Ok((input, string)) = string(input) {
        Ok((
            input,
            string.flat_map(move |string| {
                let span = string.span();
                set.map(|set| {
                    Expr::Binary(ExprBinary::new(
                        BinaryOp::HasAttr,
                        Box::new(set),
                        Box::new(string),
                        span,
                    ))
                })
            }),
        ))
    } else {
        let mut errors = Errors::new();
        errors.push(Error::expected_found(
            vec!["identifier, interpolated expression or string".into()],
            input
                .fragment
                .chars()
                .nth(0)
                .map(|c| c.to_string())
                .unwrap_or_else(|| "<EOF>".into()),
            Default::default(), // TODO: what span is this?
        ));
        Err(nom::Err::Error(errors))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::combinator::all_consuming;

    use crate::ast::{tokens::Literal, ExprFnApp};

    #[test]
    fn it_works() {
        let input = LocatedSpan::new("a b ? c");
        let (_, val) = all_consuming(membership_expr)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::Binary(ExprBinary::new(
                BinaryOp::HasAttr,
                Box::new(Expr::FnApp(ExprFnApp::new(
                    Box::new(Expr::Attr(vec!["a"].into())),
                    Box::new(Expr::Attr(vec!["b"].into())),
                    Default::default(),
                ))),
                Box::new(Expr::Attr(vec!["c"].into())),
                Default::default(),
            )))
        );
        let input = LocatedSpan::new("b ? ''a''");
        let (_, val) = all_consuming(membership_expr)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::Binary(ExprBinary::new(
                BinaryOp::HasAttr,
                Box::new(Expr::Attr(vec!["b"].into())),
                Box::new(Expr::Literal(Literal::String(
                    "a".into(),
                    Default::default(),
                ))),
                Default::default(),
            )))
        );
    }

    #[test]
    fn membership_in_list() {
        let input = LocatedSpan::new("b ? c");
        let (_, val) = all_consuming(membership_in_list_expr)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::Binary(ExprBinary::new(
                BinaryOp::HasAttr,
                Box::new(Expr::Attr(vec!["b"].into())),
                Box::new(Expr::Attr(vec!["c"].into())),
                Default::default()
            )))
        );
    }
}
