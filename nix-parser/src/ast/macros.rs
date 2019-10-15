//! Utility macros for constructing an AST from an expression.

/// Constructs an `Expr` from a Nix expression.
///
/// # Limitations
///
/// Due to limitations of `macro_rules` in Rust, certain complex expressions might need extra
/// parentheses to reduce ambiguity and aid parsing. Some expressions are impossible to parse
/// without tweaking due to Rust's tokenization rules.
///
/// Also, if the compiler complains about hitting a certain recursion limit, try adding the
/// following module attribute to the root file of your crate:
///
/// ```rust
/// #![recursion_limit = "256"]
/// ```
///
/// # Example
///
/// ```rust,edition2018
/// # #![recursion_limit = "256"]
/// # use nix_parser::nix;
/// let expr = nix!(
///     { foo, bar ? true }:
///     let
///         root = ./foo/bar;
///         enabled = true;
///         config.value = rec {
///             first = if enabled then [ 4 5 ] else [];
///             second = [ 1 2 -3 ] ++ first;
///         };
///     in
///         { inherit foo root config; }
/// );
/// ```
#[macro_export]
macro_rules! nix {
    ($($expr:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::nix_expr!($($expr)+)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_token {
    (<$($template:tt)+>) => {
        Literal::Template(["<", $($template),+, ">"].into_iter().collect(), Default::default())
    };

    (/ $($path:tt)/+) => {
        Literal::Path(["/", $(stringify!($path)),+].into_iter().collect(), Default::default())
    };

    ($prefix:tt / $($path:tt)/+) => {
        Literal::Path([stringify!($prefix), $(stringify!($path)),+].into_iter().collect(), Default::default())
    };

    (false) => {
        Literal::Boolean(false, Default::default())
    };

    (true) => {
        Literal::Boolean(true, Default::default())
    };

    (null) => {
        Literal::Null(Default::default())
    };

    (_) => {
        Ident::from("_")
    };

    ($ident:ident) => {
        Ident::from(stringify!($ident))
    };

    ($literal:expr) => {
        Literal::from($literal)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_formals {
    (@formals (_ ? $($default:tt)+) , $($rest:tt)*) => {{
        let name = $crate::nix_token!(_);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals ($name:ident ? $($default:tt)+) , $($rest:tt)*) => {{
        let name = $crate::nix_token!($name);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals (_) , $($rest:tt)*) => {{
        let name = $crate::nix_token!(_);
        let formal = Formal::new(name, None, Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals ($name:ident) , $($rest:tt)*) => {{
        let name = $crate::nix_token!($name);
        let formal = Formal::new(name, None, Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals (_ ? $($default:tt)+)) => {{
        let name = $crate::nix_token!(_);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
        ::std::iter::once(formal)
    }};

    (@formals ($name:ident ? $($default:tt)+)) => {{
        let name = $crate::nix_token!($name);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
        ::std::iter::once(formal)
    }};

    (@formals (_)) => {{
        let name = $crate::nix_token!(_);
        let formal = Formal::new(name, None, Default::default());
        ::std::iter::once(formal)
    }};

    (@formals ($name:ident)) => {{
        let name = $crate::nix_token!($name);
        let formal = Formal::new(name, None, Default::default());
        ::std::iter::once(formal)
    }};

    (@formals ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_formals!(@formals ($($prev)* $next) $($rest)*)
    };

    () => {
        ::std::iter::empty()
    };

    ($first:tt $($rest:tt)*) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::nix_formals!(@formals ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_list {
    (@elems (/ $($path:tt)/+) / $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems (/ $($path)/+ / $next) $($rest)*)
    };

    (@elems ($($path:tt)/+) / $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($path)/+ / $next) $($rest)*)
    };

    (@elems ($($expr:tt)+) $($rest:tt)*) => {{
        let first = ::std::iter::once($crate::unary!($($expr)+));
        let rest = $crate::nix_list!(@elems () $($rest)*);
        first.chain(rest)
    }};

    (@elems ()) => {
        ::std::iter::empty()
    };

    (@elems ($($prev:tt)*) / $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* / $next) $($rest)*)
    };

    (@elems ($($prev:tt)*) ! $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* ! $next) $($rest)*)
    };

    (@elems ($($prev:tt)*) - $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* - $next) $($rest)*)
    };

    (@elems ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* $next) $($rest)*)
    };

    ([$($elems:tt)*]) => {{
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        #[allow(unused_imports)]
        use $crate::ast::*;
        ExprList::new($crate::nix_list!(@elems () $($elems)*).collect(), Default::default())
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_bind {
    (inherit $($names:ident)+) => {{
        let names = vec![$($crate::nix_token!($names)),+];
        Bind::Inherit(BindInherit::new(names, Default::default()))
    }};

    (inherit ($($expr:tt)+) $($names:ident)+) => {{
        let expr = $crate::nix!($($expr)+);
        let names = vec![$($crate::nix_token!($names)),+];
        Bind::InheritExpr(BindInheritExpr::new(expr, names, Default::default()))
    }};

    ($($name:ident).+ = $($expr:tt)+) => {{
        let attr = AttrPath::new(vec![$(AttrSegment::Ident(Ident::from(stringify!($name)))),+]);
        let expr = $crate::nix!($($expr)+);
        Bind::Simple(BindSimple::new(None, attr, expr, Default::default()))
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_binds {
    (@binds ($($bind:tt)+) ; $($rest:tt)*) => {
        ::std::iter::once($crate::nix_bind!($($bind)+)).chain($crate::nix_binds!(@binds () $($rest)*))
    };

    (@binds ()) => {
        ::std::iter::empty()
    };

    (@binds ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_binds!(@binds ($($prev)* $next) $($rest)*)
    };

    ($first:tt $($rest:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::nix_binds!(@binds ($first) $($rest)*)
    }};
}

/// Returns an AST constructed by `nix!()` and also the source expression as a static string.
///
/// This is useful for testing whether the parser and the `nix!()` macro both produce the same AST.
///
/// # Example
///
/// ```rust,edition2018
/// # #![recursion_limit = "128"]
/// #
/// # use nix_parser::nix_expr_and_str;
/// # use nix_parser::ast::{tokens::Literal, Expr};
/// #
/// let (expr, s) = nix_expr_and_str!(~/foo/bar);
/// assert_eq!(expr, Expr::Literal(Literal::Path("~/foo/bar".into(), Default::default())));
/// assert_eq!(s, "~/foo/bar");
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! nix_expr_and_str {
    ($($expr:tt)+) => {
        (
            $crate::nix_expr!($($expr)+),
            stringify!($($expr)+)
                .replace("+ +", "++")
                .replace("< ", "<")
                .replace(" >", ">")
                .replace(" . ", ".")
                .replace(" ;", ";")
                .replace("- ", "-")
                .replace("! ", "!")
                .replace("/ /", "//")
                .replace(" / ", "/")
                .replace("$ ", "$")
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_expr {
    ($($expr:tt)+) => {
        $crate::function!($($expr)+)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! function {
    (@rule (_:) $($rest:tt)+) => {{
        let name = $crate::nix_token!(_);
        let body = $crate::nix!($($rest)+);
        let simple = FnDeclSimple::new(name, body, Default::default());
        Expr::from(ExprFnDecl::Simple(simple))
    }};

    (@rule ($arg:ident:) $($rest:tt)+) => {{
        let name = $crate::nix_token!($arg);
        let body = $crate::nix!($($rest)+);
        let simple = FnDeclSimple::new(name, body, Default::default());
        Expr::from(ExprFnDecl::Simple(simple))
    }};

    (@rule ({ $($formals:tt)* }:) $($rest:tt)+) => {{
        let formals = $crate::nix_formals!($($formals)*).collect();
        let body = $crate::nix!($($rest)+);
        let formals = FnDeclFormals::new(formals, None, None, body, Default::default());
        Expr::from(ExprFnDecl::Formals(formals))
    }};

    (@rule (with $($expr:tt)+) ; $($rest:tt)+) => {{
        let with = $crate::nix!($($expr)+);
        let expr = $crate::nix!($($rest)+);
        Expr::from(ExprWith::new(with, expr, Default::default()))
    }};

    (@rule (assert $($expr:tt)+) ; $($rest:tt)+) => {{
        let assert = $crate::nix!($($expr)+);
        let expr = $crate::nix!($($rest)+);
        Expr::from(ExprAssert::new(assert, expr, Default::default()))
    }};

    (@rule (let $($binds:tt)+) in $($rest:tt)+) => {{
        let binds = $crate::nix_binds!($($binds)+).collect();
        let body = $crate::nix!($($rest)+);
        Expr::from(ExprLetIn::new(binds, body, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::function!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::if_else!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::function!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! if_else {
    (@cond (if $($cond:tt)+) then $($rest:tt)+) => {{
        let cond = $crate::nix!($($cond)+);
        let (body, alt) = $crate::if_else!(@body (then) $($rest)+);
        Expr::from(ExprIf::new(cond, body, alt, Default::default()))
    }};

    (@cond ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::if_else!(@cond ($($prev)* $next) $($rest)*)
    };

    (@body (then $($body:tt)+) else $($rest:tt)+) => {{
        let body = $crate::nix!($($body)+);
        let alt = $crate::nix!($($rest)+);
        (body, alt)
    }};

    (@body ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::if_else!(@body ($($prev)* $next) $($rest)*)
    };

    (if $($rest:tt)+) => {
        $crate::if_else!(@cond (if) $($rest)+)
    };

    ($($expr:tt)+) => {
        $crate::imply!($($expr)+);
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! imply {
    (@rule ($($lhs:tt)+) -> $($rhs:tt)+) => {{
        let lhs = $crate::and!($($lhs)+);
        let rhs = $crate::imply!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Impl, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::imply!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::and!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::imply!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! and {
    (@rule ($($lhs:tt)+) && $($rhs:tt)+) => {{
        let lhs = $crate::or!($($lhs)+);
        let rhs = $crate::and!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::And, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::and!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::or!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::and!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! or {
    (@rule ($($lhs:tt)+) || $($rhs:tt)+) => {{
        let lhs = $crate::equality!($($lhs)+);
        let rhs = $crate::or!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Or, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::or!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::equality!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::or!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! equality {
    (@rule ($($lhs:tt)+) == $($rhs:tt)+) => {{
        let lhs = $crate::compare!($($lhs)+);
        let rhs = $crate::compare!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Eq, lhs, rhs, Default::default()))
    }};

    (@rule ($($lhs:tt)+) != $($rhs:tt)+) => {{
        let lhs = $crate::compare!($($lhs)+);
        let rhs = $crate::compare!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::NotEq, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::equality!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::compare!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::equality!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! compare {
    (@rule ($($lhs:tt)+) <= $($rhs:tt)+) => {{
        let lhs = $crate::update!($($lhs)+);
        let rhs = $crate::update!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::LessThanEq, lhs, rhs, Default::default()))
    }};

    (@rule ($($lhs:tt)+) < $($rhs:tt)+) => {{
        let lhs = $crate::update!($($lhs)+);
        let rhs = $crate::update!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::LessThan, lhs, rhs, Default::default()))
    }};

    (@rule ($($lhs:tt)+) >= $($rhs:tt)+) => {{
        let lhs = $crate::update!($($lhs)+);
        let rhs = $crate::update!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::GreaterThanEq, lhs, rhs, Default::default()))
    }};

    (@rule ($($lhs:tt)+) > $($rhs:tt)+) => {{
        let lhs = $crate::update!($($lhs)+);
        let rhs = $crate::update!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::GreaterThan, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::compare!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::update!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::compare!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! update {
    // Note that the update (`//`) operator is separated by a space in this macro because it also
    // happens to be the comment token in Rust. The `nix_expr_and_str!()` macro will replace these
    // occurrences with the correct `//` form in the output string.
    (@rule ($($lhs:tt)+) / / $($rhs:tt)+) => {{
        let lhs = $crate::sum!($($lhs)+);
        let rhs = $crate::update!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Update, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::update!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::sum!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::update!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! sum {
    (@rule ($($prev:tt)*) ++ $($rest:tt)*) => {
        $crate::sum!(@rule ($($prev)* ++) $($rest)*)
    };

    (@rule ($($lhs:tt)+) + $($rhs:tt)+) => {{
        let lhs = $crate::product!($($lhs)+);
        let rhs = $crate::sum!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Add, lhs, rhs, Default::default()))
    }};

    (@rule ($($lhs:tt)+) - $($rhs:tt)+) => {{
        let lhs = $crate::product!($($lhs)+);
        let rhs = $crate::sum!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Sub, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::sum!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::product!($($expr)+)
    };

    ( $($expr:tt)+ ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::sum!(@rule () $($expr)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! product {
    (@rule ($($lhs:tt)+) * $($rhs:tt)+) => {{
        let lhs = $crate::concat!($($lhs)+);
        let rhs = $crate::product!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Mul, lhs, rhs, Default::default()))
    }};

    // FIXME: This operator does not work currently because it interferes with path parsing.
    // (@rule ($($lhs:tt)+) / $($rhs:tt)+) => {{
    //     let lhs = $crate::unary!($($lhs)+);
    //     let rhs = $crate::product!($($rhs)+);
    //     Expr::from(ExprBinary::new(BinaryOp::Div, lhs, rhs, Default::default()))
    // }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::product!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::concat!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::product!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! concat {
    (@rule ($($lhs:tt)+) ++ $($rhs:tt)+) => {{
        let lhs = $crate::unary!($($lhs)+);
        let rhs = $crate::concat!($($rhs)+);
        Expr::from(ExprBinary::new(BinaryOp::Concat, lhs, rhs, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::concat!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::unary!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::concat!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! unary {
    (@rule - $($expr:tt)+) => {
        Expr::from(ExprUnary::new(UnaryOp::Neg, $crate::project!($($expr)+), Default::default()))
    };

    (@rule ! $($expr:tt)+) => {
        Expr::from(ExprUnary::new(UnaryOp::Not, $crate::project!($($expr)+), Default::default()))
    };

    (@rule $($expr:tt)+) => {
        $crate::project!($($expr)+)
    };

    ($($expr:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::unary!(@rule $($expr)+)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! project {
    (@rule ($($expr:tt)+) . $($path:ident).+ or $($fallback:tt)+) => {{
        let base = $crate::atomic!($($expr)+);
        let path = AttrPath::new(vec![$(AttrSegment::Ident(Ident::from(stringify!($path)))),+]);
        let fallback = $crate::project!($($fallback)+);
        Expr::from(ExprProj::new(base, path, Some(fallback), Default::default()))
    }};

    (@rule ($($expr:tt)+) . $($path:ident).+) => {{
        let base = $crate::atomic!($($expr)+);
        let path = AttrPath::new(vec![$(AttrSegment::Ident(Ident::from(stringify!($path)))),+]);
        Expr::from(ExprProj::new(base, path, None, Default::default()))
    }};

    (@rule ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::project!(@rule ($($prev)* $next) $($rest)*)
    };

    (@rule ($($expr:tt)+)) => {
        $crate::atomic!($($expr)+)
    };

    ( $first:tt $($rest:tt)* ) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::project!(@rule ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! atomic {
    (($($expr:tt)+)) => {
        Expr::from(ExprParen::new($crate::nix_expr!($($expr)+), Default::default()))
    };

    ({$($binds:tt)*}) => {
        Expr::Set(ExprSet::new($crate::nix_binds!($($binds)*).collect(), Default::default()))
    };

    (rec {$($binds:tt)*}) => {
        Expr::Rec(ExprRec::new($crate::nix_binds!($($binds)*).collect(), Default::default()))
    };

    (let {$($binds:tt)*}) => {
        Expr::Let(ExprLet::new($crate::nix_binds!($($binds)*).collect(), Default::default()))
    };

    ([$($expr:tt)*]) => {
        Expr::List($crate::nix_list!([$($expr)*]))
    };

    (<$($template:tt)+>) => {
        Expr::Literal($crate::nix_token!(<$($template)+>))
    };

    (/ $($path:tt)/+) => {
        Expr::Literal($crate::nix_token!(/$($path)/+))
    };

    ($prefix:tt / $($path:tt)/+) => {
        Expr::Literal($crate::nix_token!($prefix/$($path)/+))
    };

    (false) => {
        Expr::Literal($crate::nix_token!(false))
    };

    (true) => {
        Expr::Literal($crate::nix_token!(true))
    };

    (null) => {
        Expr::Literal($crate::nix_token!(null))
    };

    (_) => {
        Expr::Ident($crate::nix_token!(_))
    };

    ($attr:ident) => {
        Expr::Ident($crate::nix_token!($attr))
    };

    ($literal:expr) => {
        Expr::Literal($crate::nix_token!($literal))
    };

    ($($err:tt)*) => {
        compile_error!(stringify!($($err)*));
    };
}
