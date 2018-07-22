// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(all(feature = "extra-traits", feature = "full"))]

extern crate proc_macro2;
extern crate syn;

use proc_macro2::Span;
use syn::{Block, Expr, ExprBlock, ExprClosure, FnDecl, Ident, ItemFn, ReturnType, Visibility};
use syn::punctuated::Punctuated;

#[test]
fn test_async_fn() {
    let raw = "async fn process() {}";

    let expected = ItemFn {
        attrs: vec![],
        vis: Visibility::Inherited,
        constness: None,
        unsafety: None,
        asyncness: Some(Default::default()),
        abi: None,
        ident: Ident::new("process", Span::call_site()),
        decl: Box::new(FnDecl {
            fn_token: Default::default(),
            generics: Default::default(),
            paren_token: Default::default(),
            inputs: Punctuated::new(),
            variadic: None,
            output: ReturnType::Default,
        }),
        block: Box::new(Block {
            brace_token: Default::default(),
            stmts: vec![],
        }),
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_async_closure() {
    let raw = "async || {}";

    let expected = Expr::Closure(ExprClosure {
        attrs: vec![],
        movability: None,
        asyncness: Some(Default::default()),
        capture: None,
        or1_token: Default::default(),
        inputs: Punctuated::new(),
        or2_token:Default::default(),
        output: ReturnType::Default,
        body: Box::new(Expr::Block(ExprBlock {
            attrs: vec![],
            block: Block {
                brace_token: Default::default(),
                stmts: vec![],
            }
        })),
    });

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}
