use super::*;

use crate::{
    ast::{ExprFnDecl, FnDeclFormals, Formal},
    nix,
};

#[test]
fn issue_6() {
    let source = parse_source_file(
        r#"
{
  baz = quux;
  inherit ({foo}: foo) bar;

}
"#,
    )
    .unwrap();
    eprintln!("{:?}", source);
    let some_fn = ExprFnDecl::Formals(FnDeclFormals::new(
        vec![Formal::new("foo".into(), None, Default::default())],
        None,
        None,
        nix!(foo),
        Default::default(),
    ));
    assert_eq!(
        source.expr(),
        &nix!({
            baz = quux;
            inherit (antiquote!(some_fn.into())) bar;
        })
    );
}
