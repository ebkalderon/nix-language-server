use nom::{combinator::map, multi::many1, sequence::terminated};

use super::atomic;

use crate::{
    ast::{Expr, ExprFnApp},
    parser::{partial::Partial, tokens, IResult, LocatedSpan},
};

fn process_operands(operands: Vec<Partial<Expr>>) -> Partial<Expr> {
    let operands: Partial<Vec<_>> = operands.into_iter().collect();
    operands.map(|operands| {
        let mut itr = operands.into_iter();
        let first = itr.next().expect("there is at least one operand");
        itr.fold(first, |head, arg| {
            use crate::HasSpan;
            let span = codespan::Span::merge(head.span(), arg.span());
            Expr::FnApp(ExprFnApp::new(Box::new(head), Box::new(arg), span))
        })
    })
}

pub fn app_expr(input: LocatedSpan) -> IResult<Partial<Expr>> {
    map(many1(terminated(atomic, tokens::space)), process_operands)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::combinator::all_consuming;

    #[test]
    fn it_works() {
        let input = LocatedSpan::new("a b c");
        let (_, val) = all_consuming(app_expr)(input).unwrap();
        assert_eq!(
            val,
            Partial::from(Expr::FnApp(ExprFnApp::new(
                Box::new(Expr::FnApp(ExprFnApp::new(
                    Box::new(Expr::Attr(vec!["a"].into())),
                    Box::new(Expr::Attr(vec!["b"].into())),
                    Default::default()
                ))),
                Box::new(Expr::Attr(vec!["c"].into())),
                Default::default()
            )))
        );
    }
}
