use nom::bytes::complete::tag;

use crate::parser::error::ExpectedFoundError;
use crate::parser::{map_err, IResult, LocatedSpan};

macro_rules! define_operator {
    ($($function:ident => $op:expr),+) => {
        $(
            pub fn $function(input: LocatedSpan) -> IResult<LocatedSpan> {
                map_err(tag($op), |span, frag, _| {
                    let found = if frag.chars().next().filter(|c| c.is_alphanumeric()).is_some() {
                        frag.split_whitespace().next().map(|s| format!("`{}`", s)).unwrap()
                    } else {
                        frag.chars().next().map(|c| format!("`{}`", c)).unwrap_or_else(|| "EOF".into())
                    };

                    ExpectedFoundError::new(
                        format!("operator `{}`", $op),
                        found,
                        span,
                    )
                })(input)
            }
        )+
    };
}

define_operator! {
    op_add => "+",
    op_sub => "-",
    op_mul => "*",
    op_div => "/",
    op_eq => "==",
    op_neq => "!=",
    op_lt => "<",
    op_lte => "<=",
    op_gt => ">",
    op_gte => ">=",
    op_and => "&&",
    op_or => "||",
    op_concat => "++",
    op_update => "//",
    op_has_attr => "?",
    op_imply => "->",
    op_not => "!"
}
