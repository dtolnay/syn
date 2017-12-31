#![cfg(all(feature = "extra-traits", feature = "full"))]
#![feature(rustc_private)]

#[macro_use]
extern crate syn;
use syn::{BinOp, Expr, ExprBinary, ExprGroup, ExprLit, Lit, LitKind};
use syn::token::Group;

extern crate proc_macro2;
use proc_macro2::*;

#[macro_use]
mod macros;

mod common;

fn tt(k: TokenNode) -> TokenTree {
    TokenTree {
        span: Span::default(),
        kind: k,
    }
}

fn expr<T: Into<Expr>>(t: T) -> Expr {
    t.into()
}

fn lit<T: Into<Literal>>(t: T) -> Expr {
    Expr::Lit(ExprLit {
        attrs: Vec::new(),
        lit: Lit {
            value: LitKind::Other(t.into()),
            span: Span::default(),
        },
    })
}

#[test]
fn test_grouping() {
    let raw: TokenStream = vec![
        tt(TokenNode::Literal(Literal::i32(1))),
        tt(TokenNode::Op('+', Spacing::Alone)),
        tt(TokenNode::Group(
            Delimiter::None,
            vec![
                tt(TokenNode::Literal(Literal::i32(2))),
                tt(TokenNode::Op('+', Spacing::Alone)),
                tt(TokenNode::Literal(Literal::i32(3))),
            ].into_iter()
                .collect(),
        )),
        tt(TokenNode::Op('*', Spacing::Alone)),
        tt(TokenNode::Literal(Literal::i32(4))),
    ].into_iter()
        .collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 + 3i32  * 4i32");

    assert_eq!(
        common::parse::syn::<Expr>(raw),
        expr(ExprBinary {
            attrs: Vec::new(),
            left: Box::new(lit(Literal::i32(1))),
            op: BinOp::Add(<Token![+]>::default()),
            right: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(expr(ExprGroup {
                    attrs: Vec::new(),
                    group_token: Group::default(),
                    expr: Box::new(expr(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(lit(Literal::i32(2))),
                        op: BinOp::Add(<Token![+]>::default()),
                        right: Box::new(lit(Literal::i32(3))),
                    })),
                })),
                op: BinOp::Mul(<Token![*]>::default()),
                right: Box::new(lit(Literal::i32(4))),
            })),
        })
    );
}

#[test]
fn test_invalid_grouping() {
    let raw: TokenStream = vec![
        tt(TokenNode::Literal(Literal::i32(1))),
        tt(TokenNode::Op('+', Spacing::Alone)),
        tt(TokenNode::Group(
            Delimiter::None,
            vec![
                tt(TokenNode::Literal(Literal::i32(2))),
                tt(TokenNode::Op('+', Spacing::Alone)),
            ].into_iter()
                .collect(),
        )),
        tt(TokenNode::Literal(Literal::i32(3))),
        tt(TokenNode::Op('*', Spacing::Alone)),
        tt(TokenNode::Literal(Literal::i32(4))),
    ].into_iter()
        .collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 +  3i32 * 4i32");

    assert_eq!(
        common::parse::syn::<Expr>(raw),
        expr(ExprBinary {
            attrs: Vec::new(),
            left: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(lit(Literal::i32(1))),
                op: BinOp::Add(<Token![+]>::default()),
                right: Box::new(lit(Literal::i32(2))),
            })),
            op: BinOp::Add(<Token![+]>::default()),
            right: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(lit(Literal::i32(3))),
                op: BinOp::Mul(<Token![*]>::default()),
                right: Box::new(lit(Literal::i32(4))),
            })),
        })
    );
}
