#![cfg(feature = "full")]

extern crate syn;
use syn::*;

#[test]
fn test_box() {
    let raw = "box 0";

    let expected = Expr::Box(Box::new(
        Expr::Lit(Lit::Int(0, IntTy::Unsuffixed))
    ));

    assert_eq!(expected, parse_expr(raw).unwrap());
}
