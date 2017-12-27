#![cfg(all(feature = "extra-traits", feature = "full"))]

#![feature(rustc_private)]

#[macro_use]
extern crate syn;
use syn::{Expr, ExprKind, ExprGroup, ExprBinary, Lit, LitKind, BinOp};
use syn::token::Group;

extern crate proc_macro2;
use proc_macro2::*;

mod common;

fn tt(k: TokenNode) -> TokenTree {
    TokenTree {
        span: Span::default(),
        kind: k,
    }
}

fn expr<T: Into<ExprKind>>(t: T) -> Expr {
    t.into().into()
}

fn lit<T: Into<Literal>>(t: T) -> Expr {
    expr(Lit {
        value: LitKind::Other(t.into()),
        span: Span::default(),
    })
}

#[test]
fn test_grouping() {
    let raw: TokenStream = vec![
        tt(TokenNode::Literal(Literal::i32(1))),
        tt(TokenNode::Op('+', Spacing::Alone)),
        tt(TokenNode::Group(Delimiter::None, vec![
            tt(TokenNode::Literal(Literal::i32(2))),
            tt(TokenNode::Op('+', Spacing::Alone)),
            tt(TokenNode::Literal(Literal::i32(3))),
        ].into_iter().collect())),
        tt(TokenNode::Op('*', Spacing::Alone)),
        tt(TokenNode::Literal(Literal::i32(4))),
    ].into_iter().collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 + 3i32  * 4i32");

    assert_eq!(common::parse::syn::<Expr>(raw), expr(ExprBinary {
        left: Box::new(lit(Literal::i32(1))),
        op: BinOp::Add(<Token![+]>::default()),
        right: Box::new(expr(ExprBinary {
            left: Box::new(expr(ExprGroup {
                group_token: Group::default(),
                expr: Box::new(expr(ExprBinary {
                    left: Box::new(lit(Literal::i32(2))),
                    op: BinOp::Add(<Token![+]>::default()),
                    right: Box::new(lit(Literal::i32(3))),
                })),
            })),
            op: BinOp::Mul(<Token![*]>::default()),
            right: Box::new(lit(Literal::i32(4))),
        })),
    }));
}

#[test]
fn test_invalid_grouping() {
    let raw: TokenStream = vec![
        tt(TokenNode::Literal(Literal::i32(1))),
        tt(TokenNode::Op('+', Spacing::Alone)),
        tt(TokenNode::Group(Delimiter::None, vec![
            tt(TokenNode::Literal(Literal::i32(2))),
            tt(TokenNode::Op('+', Spacing::Alone)),
        ].into_iter().collect())),
        tt(TokenNode::Literal(Literal::i32(3))),
        tt(TokenNode::Op('*', Spacing::Alone)),
        tt(TokenNode::Literal(Literal::i32(4))),
    ].into_iter().collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 +  3i32 * 4i32");

    assert_eq!(common::parse::syn::<Expr>(raw), expr(ExprBinary {
        left: Box::new(expr(ExprBinary {
            left: Box::new(lit(Literal::i32(1))),
            op: BinOp::Add(<Token![+]>::default()),
            right: Box::new(lit(Literal::i32(2))),
        })),
        op: BinOp::Add(<Token![+]>::default()),
        right: Box::new(expr(ExprBinary {
            left: Box::new(lit(Literal::i32(3))),
            op: BinOp::Mul(<Token![*]>::default()),
            right: Box::new(lit(Literal::i32(4))),
        })),
    }));
}
