extern crate syn;
extern crate syntex_syntax;

use self::syntex_syntax::ast;
use self::syntex_syntax::ptr::P;
use self::syntex_syntax::parse::{self, ParseSess};
use self::syntex_syntax::codemap::FilePathMapping;

use std::panic;

pub fn syntex_expr(input: &str) -> Option<P<ast::Expr>> {
    match panic::catch_unwind(|| {
        let sess = ParseSess::new(FilePathMapping::empty());
        sess.span_diagnostic.set_continue_after_error(false);
        let e = parse::parse_expr_from_source_str(
            "test_precedence".to_string(),
            input.to_string(),
            &sess,
        );
        Some(match e {
            Ok(expr) => expr,
            Err(mut diagnostic) => {
                diagnostic.emit();;
                return None
            }
        })
    }) {
        Ok(Some(e)) => Some(e),
        Ok(None) => {
            None
        }
        Err(_) => {
            errorf!("syntex paniced\n");
            None
        }
    }
}

pub fn syn_expr(input: &str) -> Option<syn::Expr> {
    match input.parse::<syn::Expr>() {
        Ok(e) => Some(e),
        Err(msg) => {
            errorf!("syn failed to parse\n{:?}\n", msg);
            None
        }
    }
}
