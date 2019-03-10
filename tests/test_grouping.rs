#![recursion_limit = "1024"]

#[macro_use]
extern crate syn;
use syn::token::Group;
use syn::{BinOp, Expr, ExprBinary, ExprGroup, ExprLit, Lit};

extern crate proc_macro2;
use proc_macro2::*;

mod features;

fn expr<T: Into<Expr>>(t: T) -> Expr {
    t.into()
}

fn lit<T: Into<Literal>>(t: T) -> Expr {
    Expr::Lit(ExprLit {
        attrs: Vec::new(),
        lit: Lit::new(t.into()),
    })
}

#[test]
fn test_grouping() {
    let raw: TokenStream = vec![
        TokenTree::Literal(Literal::i32_suffixed(1)),
        TokenTree::Punct(Punct::new('+', Spacing::Alone)),
        TokenTree::Group(proc_macro2::Group::new(
            Delimiter::None,
            vec![
                TokenTree::Literal(Literal::i32_suffixed(2)),
                TokenTree::Punct(Punct::new('+', Spacing::Alone)),
                TokenTree::Literal(Literal::i32_suffixed(3)),
            ]
            .into_iter()
            .collect(),
        )),
        TokenTree::Punct(Punct::new('*', Spacing::Alone)),
        TokenTree::Literal(Literal::i32_suffixed(4)),
    ]
    .into_iter()
    .collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 + 3i32  * 4i32");

    assert_eq!(
        syn::parse2::<syn::Expr>(raw).unwrap(),
        expr(ExprBinary {
            attrs: Vec::new(),
            left: Box::new(lit(Literal::i32_suffixed(1))),
            op: BinOp::Add(<Token![+]>::default()),
            right: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(expr(ExprGroup {
                    attrs: Vec::new(),
                    group_token: Group::default(),
                    expr: Box::new(expr(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(lit(Literal::i32_suffixed(2))),
                        op: BinOp::Add(<Token![+]>::default()),
                        right: Box::new(lit(Literal::i32_suffixed(3))),
                    })),
                })),
                op: BinOp::Mul(<Token![*]>::default()),
                right: Box::new(lit(Literal::i32_suffixed(4))),
            })),
        })
    );
}
