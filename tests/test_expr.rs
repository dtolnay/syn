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

#[test]
fn test_unnamed_loop() {
    let block = match parse_expr("{ ( 1, 3, 8 ) }").unwrap() {
        Expr::Block(b) => b,
        _ => panic!("Could not run test_unnamed_loop: error in block parse."),
    };

    let raw = "loop {(1, 3, 8 )}";

    let expected = Expr::Loop(block, None);

    assert_eq!(expected, parse_expr(raw).unwrap());
}

#[test]
fn test_named_loop() {
    let block = match parse_expr("{ ( 1, 5, 9, 11) }").unwrap() {
        Expr::Block(b) => b,
        _ => panic!("Could not run named_loop: error in block parse."),
    };

    let raw = "' test : loop{(1, 5, 9, 11)}";

    let expected = Expr::Loop(block, Some("'test".into()));

    assert_eq!(expected, parse_expr(raw).unwrap());
}

#[test]
// Ignore test until bool parsing is available
#[ignore]
fn test_unnamed_while() {
    let block = match parse_expr("{ ( 1, 3, 8 ) }").unwrap() {
        Expr::Block(b) => b,
        _ => panic!("Could not run test_unnamed_while: error in block parse."),
    };

    let raw = "while true {(1, 3, 8 )}";

    let expected = Expr::While(Box::new(Expr::Lit(Lit::Bool(true))), block, None);

    assert_eq!(expected, parse_expr(raw).unwrap());
}

#[test]
// Ignore test until bool parsing is available
#[ignore]
fn test_named_while() {
    let block = match parse_expr("{ ( 1, 5, 9, 11) }").unwrap() {
        Expr::Block(b) => b,
        _ => panic!("Could not run named_while: error in block parse."),
    };

    let raw = "' test :  while true {(1, 5, 9, 11)}";

    let expected = Expr::While(Box::new(Expr::Lit(Lit::Bool(true))), block, Some("'test".into()));

    assert_eq!(expected, parse_expr(raw).unwrap());
}
