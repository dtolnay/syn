extern crate proc_macro2;
extern crate syn;

use std::str::FromStr;

use proc_macro2::TokenStream;
use syn::{Expr, parse2};

#[test]
fn test_expr_parse() {
    macro_rules! test_equiv {
        ($code:ident, $old:ident, $new:ident) => (
            let tt = TokenStream::from_str($code).unwrap();
            //println!("--- tt ---\n{:?}\n", tt);
            let ast1: Expr = parse2(tt.clone()).unwrap();
            //println!("--- ast1 ---\n{:?}\n", ast1);
            let ast2: syn::$new = parse2(tt).unwrap();
            //println!("--- ast2 ---\n{:?}\n", ast2);
            assert_eq!(ast1, Expr::$old(ast2));
        )
    }

    let code = "match foo { Bar::Qux => (), _ => panic!() }";
    test_equiv!(code, Match, ExprMatch);

    let code = "..100u32";
    test_equiv!(code, Range, ExprRange);
}
