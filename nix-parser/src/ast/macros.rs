//! Utility macros for constructing an AST from an expression.

/// Construct an AST `Expr` from a Nix expression.
///
/// # Limitations
///
/// Due to limitations of `macro_rules` in Rust, certain complex expressions might need extra
/// parentheses to reduce ambiguity and aid parsing.
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
/// let expr = nix!(./foo/bar);
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

    ($($ipath:ident).+) => {
        IdentPath::from(vec![$(stringify!($ipath)),+])
    };

    ($literal:expr) => {
        Literal::from($literal)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! nix_expr {
    ($($expr:tt)+) => {
        $crate::atomic!($($expr)+)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! atomic {
    (($($expr:tt)+)) => {
        Expr::Paren(ExprParen::new(Box::new($crate::nix_expr!($($expr)+)), Default::default()))
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
        Expr::Attr(IdentPath::from(vec![$crate::nix_token!(_)]))
    };

    ($($attr:ident).+) => {
        Expr::Attr(IdentPath::from(vec![$(stringify!($ipath)),+]))
    };

    ($literal:expr) => {
        Expr::Literal($crate::nix_token!($literal))
    }
}
