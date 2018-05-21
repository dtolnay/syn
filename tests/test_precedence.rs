// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(all(feature = "full", feature = "fold"))]
#![feature(rustc_private)]

//! The tests in this module do the following:
//!
//! 1. Parse a given expression in both `syn` and `libsyntax`.
//! 2. Fold over the expression adding brackets around each subexpression (with
//!    some complications - see the `syn_brackets` and `libsyntax_brackets`
//!    methods).
//! 3. Serialize the `syn` expression back into a string, and re-parse it with
//!    `libsyntax`.
//! 4. Respan all of the expressions, replacing the spans with the default spans.
//! 5. Compare the expressions with one another, if they are not equal fail.

#[macro_use]
extern crate quote;
extern crate rayon;
extern crate syn;
extern crate syntax;
extern crate walkdir;

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use syntax::ast;
use syntax::ptr::P;
use walkdir::{DirEntry, WalkDir};

use std::fs::File;
use std::io::Read;
use std::process;
use std::sync::atomic::{AtomicUsize, Ordering};

use common::{parse, respan};

#[macro_use]
mod macros;

#[allow(dead_code)]
mod common;

/// Test some pre-set expressions chosen by us.
#[test]
fn test_simple_precedence() {
    const EXPRS: &[&str] = &[
        "1 + 2 * 3 + 4",
        "1 + 2 * ( 3 + 4 )",
        "{ for i in r { } *some_ptr += 1; }",
        "{ loop { break 5; } }",
        "{ if true { () }.mthd() }",
        "{ for i in unsafe { 20 } { } }",
    ];

    let mut failed = 0;

    for input in EXPRS {
        let expr = if let Some(expr) = parse::syn_expr(input) {
            expr
        } else {
            failed += 1;
            continue;
        };

        let pf = match test_expressions(vec![expr]) {
            (1, 0) => "passed",
            (0, 1) => {
                failed += 1;
                "failed"
            }
            _ => unreachable!(),
        };
        errorf!("=== {}: {}\n", input, pf);
    }

    if failed > 0 {
        panic!("Failed {} tests", failed);
    }
}

/// Test expressions from rustc, like in `test_round_trip`.
#[test]
fn test_rustc_precedence() {
    common::check_min_stack();
    common::clone_rust();
    let abort_after = common::abort_after();
    if abort_after == 0 {
        panic!("Skipping all precedence tests");
    }

    let passed = AtomicUsize::new(0);
    let failed = AtomicUsize::new(0);

    WalkDir::new("tests/rust")
        .sort_by(|a, b| a.file_name().cmp(b.file_name()))
        .into_iter()
        .filter_entry(common::base_dir_filter)
        .collect::<Result<Vec<DirEntry>, walkdir::Error>>()
        .unwrap()
        .into_par_iter()
        .for_each(|entry| {
            let path = entry.path();
            if path.is_dir() {
                return;
            }

            // Our version of `libsyntax` can't parse this tests
            if path
                .to_str()
                .unwrap()
                .ends_with("optional_comma_in_match_arm.rs")
            {
                return;
            }

            let mut file = File::open(path).unwrap();
            let mut content = String::new();
            file.read_to_string(&mut content).unwrap();

            let (l_passed, l_failed) = match syn::parse_file(&content) {
                Ok(file) => {
                    let exprs = collect_exprs(file);
                    test_expressions(exprs)
                }
                Err(msg) => {
                    errorf!("syn failed to parse\n{:?}\n", msg);
                    (0, 1)
                }
            };

            errorf!(
                "=== {}: {} passed | {} failed\n",
                path.display(),
                l_passed,
                l_failed
            );

            passed.fetch_add(l_passed, Ordering::SeqCst);
            let prev_failed = failed.fetch_add(l_failed, Ordering::SeqCst);

            if prev_failed + l_failed >= abort_after {
                process::exit(1);
            }
        });

    let passed = passed.load(Ordering::SeqCst);
    let failed = failed.load(Ordering::SeqCst);

    errorf!("\n===== Precedence Test Results =====\n");
    errorf!("{} passed | {} failed\n", passed, failed);

    if failed > 0 {
        panic!("{} failures", failed);
    }
}

fn test_expressions(exprs: Vec<syn::Expr>) -> (usize, usize) {
    let mut passed = 0;
    let mut failed = 0;

    syntax::with_globals(|| {
        for expr in exprs {
            let raw = quote!(#expr).to_string();

            let libsyntax_ast = if let Some(e) = libsyntax_parse_and_rewrite(&raw) {
                e
            } else {
                failed += 1;
                errorf!("\nFAIL - libsyntax failed to parse raw\n");
                continue;
            };

            let syn_expr = syn_brackets(expr);
            let syn_ast = if let Some(e) = parse::libsyntax_expr(&quote!(#syn_expr).to_string()) {
                e
            } else {
                failed += 1;
                errorf!("\nFAIL - libsyntax failed to parse bracketed\n");
                continue;
            };

            let syn_ast = respan::respan_expr(syn_ast);
            let libsyntax_ast = respan::respan_expr(libsyntax_ast);

            if syn_ast == libsyntax_ast {
                passed += 1;
            } else {
                failed += 1;
                errorf!("\nFAIL\n{:?}\n!=\n{:?}\n", syn_ast, libsyntax_ast);
            }
        }
    });

    (passed, failed)
}

fn libsyntax_parse_and_rewrite(input: &str) -> Option<P<ast::Expr>> {
    parse::libsyntax_expr(input).and_then(libsyntax_brackets)
}

/// Wrap every expression which is not already wrapped in parens with parens, to
/// reveal the precidence of the parsed expressions, and produce a stringified form
/// of the resulting expression.
///
/// This method operates on libsyntax objects.
fn libsyntax_brackets(libsyntax_expr: P<ast::Expr>) -> Option<P<ast::Expr>> {
    use syntax::ast::{Expr, ExprKind, Field, Mac, Pat, Stmt, StmtKind, Ty};
    use syntax::ext::quote::rt::DUMMY_SP;
    use syntax::fold::{self, Folder};
    use syntax::util::small_vector::SmallVector;
    use syntax::util::ThinVec;

    fn expr(node: ExprKind) -> P<Expr> {
        P(Expr {
            id: ast::DUMMY_NODE_ID,
            node,
            span: DUMMY_SP,
            attrs: ThinVec::new(),
        })
    }

    struct BracketsFolder {
        failed: bool,
    };
    impl Folder for BracketsFolder {
        fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
            e.map(|e| Expr {
                node: match e.node {
                    ExprKind::Paren(inner) => {
                        ExprKind::Paren(inner.map(|e| fold::noop_fold_expr(e, self)))
                    }
                    ExprKind::If(..) | ExprKind::Block(..) | ExprKind::IfLet(..) => {
                        return fold::noop_fold_expr(e, self);
                    }
                    node => ExprKind::Paren(expr(node).map(|e| fold::noop_fold_expr(e, self))),
                },
                ..e
            })
        }

        fn fold_field(&mut self, f: Field) -> Field {
            Field {
                expr: if f.is_shorthand {
                    f.expr.map(|e| fold::noop_fold_expr(e, self))
                } else {
                    self.fold_expr(f.expr)
                },
                ..f
            }
        }

        // We don't want to look at expressions that might appear in patterns or
        // types yet. We'll look into comparing those in the future. For now
        // focus on expressions appearing in other places.
        fn fold_pat(&mut self, pat: P<Pat>) -> P<Pat> {
            pat
        }

        fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
            ty
        }

        fn fold_stmt(&mut self, stmt: Stmt) -> SmallVector<Stmt> {
            let node = match stmt.node {
                // Don't wrap toplevel expressions in statements.
                StmtKind::Expr(e) => StmtKind::Expr(e.map(|e| fold::noop_fold_expr(e, self))),
                StmtKind::Semi(e) => StmtKind::Semi(e.map(|e| fold::noop_fold_expr(e, self))),
                s => s,
            };

            SmallVector::one(Stmt { node, ..stmt })
        }

        fn fold_mac(&mut self, mac: Mac) -> Mac {
            // By default when folding over macros, libsyntax panics. This is
            // because it's usually not what you want, you want to run after
            // macro expansion. We do want to do that (syn doesn't do macro
            // expansion), so we implement fold_mac to just return the macro
            // unchanged.
            mac
        }
    }

    let mut folder = BracketsFolder { failed: false };
    let e = folder.fold_expr(libsyntax_expr);
    if folder.failed {
        None
    } else {
        Some(e)
    }
}

/// Wrap every expression which is not already wrapped in parens with parens, to
/// reveal the precedence of the parsed expressions, and produce a stringified form
/// of the resulting expression.
fn syn_brackets(syn_expr: syn::Expr) -> syn::Expr {
    use syn::fold::*;
    use syn::*;

    fn paren(folder: &mut ParenthesizeEveryExpr, mut node: Expr) -> Expr {
        let attrs = node.replace_attrs(Vec::new());
        Expr::Paren(ExprParen {
            attrs,
            expr: Box::new(fold_expr(folder, node)),
            paren_token: token::Paren::default(),
        })
    }

    struct ParenthesizeEveryExpr;
    impl Fold for ParenthesizeEveryExpr {
        fn fold_expr(&mut self, expr: Expr) -> Expr {
            match expr {
                Expr::Group(_) => unreachable!(),
                Expr::Paren(p) => paren(self, *p.expr),
                Expr::If(..) | Expr::Unsafe(..) | Expr::Block(..) | Expr::IfLet(..) => {
                    fold_expr(self, expr)
                }
                node => paren(self, node),
            }
        }

        fn fold_stmt(&mut self, stmt: Stmt) -> Stmt {
            match stmt {
                // Don't wrap toplevel expressions in statements.
                Stmt::Expr(e) => Stmt::Expr(fold_expr(self, e)),
                Stmt::Semi(e, semi) => Stmt::Semi(fold_expr(self, e), semi),
                s => s,
            }
        }

        // We don't want to look at expressions that might appear in patterns or
        // types yet. We'll look into comparing those in the future. For now
        // focus on expressions appearing in other places.
        fn fold_pat(&mut self, pat: Pat) -> Pat {
            pat
        }

        fn fold_type(&mut self, ty: Type) -> Type {
            ty
        }
    }

    let mut folder = ParenthesizeEveryExpr;
    folder.fold_expr(syn_expr)
}

/// Walk through a crate collecting all expressions we can find in it.
fn collect_exprs(file: syn::File) -> Vec<syn::Expr> {
    use syn::fold::*;
    use syn::punctuated::Punctuated;
    use syn::*;

    struct CollectExprs(Vec<Expr>);
    impl Fold for CollectExprs {
        fn fold_expr(&mut self, expr: Expr) -> Expr {
            self.0.push(expr);

            Expr::Tuple(ExprTuple {
                attrs: vec![],
                elems: Punctuated::new(),
                paren_token: token::Paren::default(),
            })
        }
    }

    let mut folder = CollectExprs(vec![]);
    folder.fold_file(file);
    folder.0
}
