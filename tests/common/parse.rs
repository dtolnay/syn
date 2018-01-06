// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate proc_macro2;
extern crate syn;
extern crate syntax;
extern crate syntax_pos;

use self::syntax::ast;
use self::syntax::ptr::P;
use self::syntax::parse::{self, ParseSess};
use self::syntax::codemap::FilePathMapping;
use self::syntax_pos::FileName;

use std::panic;

use self::syn::buffer::TokenBuffer;
use self::syn::synom::Synom;

pub fn libsyntax_expr(input: &str) -> Option<P<ast::Expr>> {
    match panic::catch_unwind(|| {
        let sess = ParseSess::new(FilePathMapping::empty());
        sess.span_diagnostic.set_continue_after_error(false);
        let e = parse::parse_expr_from_source_str(
            FileName::Custom("test_precedence".to_string()),
            input.to_string(),
            &sess,
        );
        Some(match e {
            Ok(expr) => expr,
            Err(mut diagnostic) => {
                diagnostic.emit();;
                return None;
            }
        })
    }) {
        Ok(Some(e)) => Some(e),
        Ok(None) => None,
        Err(_) => {
            errorf!("libsyntax panicked\n");
            None
        }
    }
}

pub fn syn_expr(input: &str) -> Option<syn::Expr> {
    match syn::parse_str(input) {
        Ok(e) => Some(e),
        Err(msg) => {
            errorf!("syn failed to parse\n{:?}\n", msg);
            None
        }
    }
}

pub fn syn<T: Synom>(tokens: proc_macro2::TokenStream) -> T {
    let buf = TokenBuffer::new(tokens);
    let result = T::parse(buf.begin());
    match result {
        Ok((t, rest)) => {
            if rest.eof() {
                t
            } else if rest == buf.begin() {
                panic!("failed to parse anything")
            } else {
                panic!("failed to parse all tokens")
            }
        }
        Err(err) => panic!("failed to parse: {}", err),
    }
}
