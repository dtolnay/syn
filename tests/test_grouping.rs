// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(all(feature = "extra-traits", feature = "full"))]
#![feature(rustc_private)]

#[macro_use]
extern crate syn;
use syn::token::Group;
use syn::{BinOp, Expr, ExprBinary, ExprGroup, ExprLit, Lit};

extern crate proc_macro2;
use proc_macro2::*;

#[macro_use]
mod macros;

mod common;

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
            ].into_iter()
                .collect(),
        )),
        TokenTree::Punct(Punct::new('*', Spacing::Alone)),
        TokenTree::Literal(Literal::i32_suffixed(4)),
    ].into_iter()
        .collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 + 3i32  * 4i32");

    assert_eq!(
        common::parse::syn::<Expr>(raw),
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

#[test]
fn test_invalid_grouping() {
    let raw: TokenStream = vec![
        TokenTree::Literal(Literal::i32_suffixed(1)),
        TokenTree::Punct(Punct::new('+', Spacing::Alone)),
        TokenTree::Group(proc_macro2::Group::new(
            Delimiter::None,
            vec![
                TokenTree::Literal(Literal::i32_suffixed(2)),
                TokenTree::Punct(Punct::new('+', Spacing::Alone)),
            ].into_iter()
                .collect(),
        )),
        TokenTree::Literal(Literal::i32_suffixed(3)),
        TokenTree::Punct(Punct::new('*', Spacing::Alone)),
        TokenTree::Literal(Literal::i32_suffixed(4)),
    ].into_iter()
        .collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 +  3i32 * 4i32");

    assert_eq!(
        common::parse::syn::<Expr>(raw),
        expr(ExprBinary {
            attrs: Vec::new(),
            left: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(lit(Literal::i32_suffixed(1))),
                op: BinOp::Add(<Token![+]>::default()),
                right: Box::new(lit(Literal::i32_suffixed(2))),
            })),
            op: BinOp::Add(<Token![+]>::default()),
            right: Box::new(expr(ExprBinary {
                attrs: Vec::new(),
                left: Box::new(lit(Literal::i32_suffixed(3))),
                op: BinOp::Mul(<Token![*]>::default()),
                right: Box::new(lit(Literal::i32_suffixed(4))),
            })),
        })
    );
}
