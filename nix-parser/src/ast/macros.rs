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
/// ```rust,ignore
/// #![recursion_limit = "128"]
/// ```
///
/// # Example
///
/// ```rust,edition2018
/// # #![recursion_limit = "128"]
/// # use nix_parser::nix;
/// let expr = nix!(
///     { foo, bar ? true }:
///     let
///         root = ./foo/bar;
///         config.value = {
///             first = true;
///             second = if bar then [1 2 -3] else [];
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

    ($($path:ident).+) => {
        AttrPath::new(vec![$(AttrSegment::Ident(Ident::from(stringify!($path)))),+])
    };

    ($literal:expr) => {
        Literal::from($literal)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_formals {
    (@formals ($name:ident ? $($default:tt)+) , $($rest:tt)*) => {{
        let name = $crate::nix_token!($name);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals ($name:ident) , $($rest:tt)*) => {{
        let name = $crate::nix_token!($name);
        let formal = Formal::new(name, None, Default::default());
        ::std::iter::once(formal).chain($crate::nix_formals!($($rest)*))
    }};

    (@formals ($name:ident ? $($default:tt)+)) => {{
        let name = $crate::nix_token!($name);
        let default = $crate::nix!($($default)+);
        let formal = Formal::new(name, Some(default), Default::default());
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
    (@elems (/ $($path:ident)/+) $next:tt $($rest:tt)*) => {{
        let first = $crate::nix_list!(@elems (/ $($path)/+));
        let rest = $crate::nix_list!(@elems ($next) $($rest)*);
        first.chain(rest)
    }};

    (@elems ($prefix:tt / $($path:tt)/+), $next:tt $($rest:tt)*) => {{
        let first = $crate::nix_list!(@elems ($prefix / $($path)/+));
        let rest = $crate::nix_list!(@elems ($next) $($rest)*);
        first.chain(rest)
    }};

    (@elems (- $expr:tt) $next:tt $($rest:tt)*) => {{
        let first = $crate::nix_list!(@elems (- $expr));
        let rest = $crate::nix_list!(@elems ($next) $($rest)*);
        first.chain(rest)
    }};

    (@elems (! $expr:tt) $next:tt $($rest:tt)*) => {{
        let first = $crate::nix_list!(@elems (! $expr));
        let rest = $crate::nix_list!(@elems ($next) $($rest)*);
        first.chain(rest)
    }};

    (@elems ($($prev:tt)*) / $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* / $next) $($rest)*)
    };

    (@elems ($($prev:tt)*) - $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* - $next) $($rest)*)
    };

    (@elems ($($prev:tt)*) ! $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* ! $next) $($rest)*)
    };

    (@elems ($expr:tt) $next:tt $($rest:tt)*) => {{
        let first = $crate::nix_list!(@elems ($expr));
        let rest = $crate::nix_list!(@elems ($next) $($rest)*);
        first.chain(rest)
    }};

    (@elems ($($expr:tt)+)) => {
        ::std::iter::once($crate::unary!($($expr)+))
    };

    (@elems ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_list!(@elems ($($prev)* $next) $($rest)*)
    };

    ([ ]) => {{
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        #[allow(unused_imports)]
        use $crate::ast::*;
        ExprList::new(Vec::new(), Default::default())
    }};

    ([$first:tt $($rest:tt)*]) => {{
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        #[allow(unused_imports)]
        use $crate::ast::*;
        ExprList::new($crate::nix_list!(@elems ($first) $($rest)*).collect(), Default::default())
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
macro_rules! nix_let_in {
    (@binds ($($bind:tt)+) ; $($rest:tt)+) => {
        ::std::iter::once($crate::nix_bind!($($bind)+)).chain($crate::nix_set!(@binds $($rest)+))
    };

    (@binds ($($bind:tt)+) ;) => {
        ::std::iter::once($crate::nix_bind!($($bind)+))
    };

    (@binds ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_let_in!(@binds ($($prev)* $next) $($rest)*)
    };

    ($first:tt $($rest:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::nix_let_in!(@binds ($first) $($rest)*)
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_set {
    ({ }) => {
        ExprSet::new(Vec::new(), Default::default())
    };

    ({ $($binds:tt)+ }) => {
        ExprSet::new($crate::nix_set!(@binds $($binds)*).collect(), Default::default())
    };

    (@binds ($($bind:tt)+) ; $($rest:tt)+) => {
        ::std::iter::once($crate::nix_bind!($($bind)+)).chain($crate::nix_set!(@binds $($rest)+))
    };

    (@binds ($($bind:tt)+) ;) => {
        ::std::iter::once($crate::nix_bind!($($bind)+))
    };

    (@binds ($($prev:tt)*) $next:tt $($rest:tt)*) => {
        $crate::nix_set!(@binds ($($prev)* $next) $($rest)*)
    };

    (@binds $first:tt $($rest:tt)*) => {{
        #[allow(unused_imports)]
        use $crate::ast::*;
        #[allow(unused_imports)]
        use $crate::ast::tokens::*;
        $crate::nix_set!(@binds ($first) $($rest)*)
    }};
}

/// Returns an AST constructed by `nix!()` and also the source expression as a static string.
///
/// This is useful for testing whether the parser and the `nix!()` macro both produce the same AST.
///
/// # Example
///
/// ```rust,edition2018
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
        let binds = $crate::nix_let_in!($($binds)+).collect();
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
        $crate::unary!($($expr)+);
    };
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
        let base = $crate::nix!($($expr)+);
        let path = AttrPath::new(vec![$(AttrSegment::Ident(Ident::from(stringify!($path)))),+]);
        let fallback = $crate::project!($($fallback)+);
        Expr::from(ExprProj::new(base, path, Some(fallback), Default::default()))
    }};

    (@rule ($($expr:tt)+) . $($path:ident).+) => {{
        let base = $crate::nix!($($expr)+);
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
        Expr::Set($crate::nix_set!({$($binds)*}))
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
