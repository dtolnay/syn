#![cfg(all(feature = "full", feature = "fold"))]

//! The tests in this module do the following:
//!
//! 1. Parse a given expression in both `syn` and `syntex`.
//! 2. Fold over the expression adding brackets around each subexpression (with
//!    some complications - see the `syn_brackets` and `syntex_brackets`
//!    methods).
//! 3. Serialize the `syn` expression back into a string, and re-parse it with
//!    `syntex`.
//! 4. Respan all of the expressions, replacing the spans with the default spans.
//! 5. Compare the expressions with one another, if they are not equal fail.

#[macro_use]
extern crate quote;
extern crate syn;
extern crate synom;
extern crate syntex_syntax;
extern crate walkdir;

use syntex_syntax::ast;
use syntex_syntax::ptr::P;

use common::{respan, parse};

#[allow(dead_code)]
#[macro_use]
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
    use walkdir::{WalkDir, WalkDirIterator};
    use std::fs::File;
    use std::io::Read;

    common::check_min_stack();
    common::clone_rust();
    let abort_after = common::abort_after();
    if abort_after == 0 {
        panic!("Skipping all precedence tests");
    }

    let mut passed = 0;
    let mut failed = 0;

    let walk = WalkDir::new("tests/rust").sort_by(|a, b| a.cmp(b));
    for entry in walk.into_iter().filter_entry(common::base_dir_filter) {
        let entry = entry.unwrap();

        let path = entry.path();
        if path.is_dir() {
            continue;
        }

        // Our version of `syntex_syntax` can't parse this tests
        if path.to_str().unwrap().ends_with("optional_comma_in_match_arm.rs") {
            continue
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
                failed += 1;
                (0, 1)
            }
        };

        passed += l_passed;
        failed += l_failed;

        errorf!("=== {}: {} passed | {} failed\n", path.display(), l_passed, l_failed);

        if failed >= abort_after {
            errorf!("Aborting Immediately due to ABORT_AFTER_FAILURE\n");
            break;
        }
    }

    errorf!("\n===== Precedence Test Results =====\n");
    errorf!("{} passed | {} failed\n", passed, failed);


    if failed > 0 {
        panic!("{} failures", failed);
    }
}

fn test_expressions(exprs: Vec<syn::Expr>) -> (u32, u32) {
    let mut passed = 0;
    let mut failed = 0;

    for expr in exprs {
        let raw = quote!(#expr).to_string();

        let syntex_ast = if let Some(e) = syntex_parse_and_rewrite(&raw) {
            e
        } else {
            failed += 1;
            errorf!("\nFAIL - syntex failed to parse raw\n");
            continue;
        };

        let syn_expr = syn_brackets(expr);
        let syn_ast = if let Some(e) = parse::syntex_expr(&quote!(#syn_expr).to_string()) {
            e
        } else {
            failed += 1;
            errorf!("\nFAIL - syntex failed to parse bracketed\n");
            continue;
        };

        let syn_ast = respan::respan_expr(syn_ast);
        let syntex_ast = respan::respan_expr(syntex_ast);

        if syn_ast == syntex_ast {
            passed += 1;
        } else {
            failed += 1;
            errorf!("\nFAIL\n{:?}\n!=\n{:?}\n", syn_ast, syntex_ast);
        }
    }

    (passed, failed)
}

fn syntex_parse_and_rewrite(input: &str) -> Option<P<ast::Expr>> {
    parse::syntex_expr(input).and_then(|e| syntex_brackets(e))
}

/// Wrap every expression which is not already wrapped in parens with parens, to
/// reveal the precidence of the parsed expressions, and produce a stringified form
/// of the resulting expression.
///
/// This method operates on syntex objects.
fn syntex_brackets(syntex_expr: P<ast::Expr>) -> Option<P<ast::Expr>> {
    use syntex_syntax::ast::{Expr, ExprKind, Mac, Stmt, StmtKind, Pat, Ty, Field};
    use syntex_syntax::fold::{self, Folder};
    use syntex_syntax::util::ThinVec;
    use syntex_syntax::util::small_vector::SmallVector;
    use syntex_syntax::ext::quote::rt::DUMMY_SP;
    use syntex_syntax::codemap;

    fn expr(node: ExprKind) -> P<Expr> {
        P(Expr {
            id: ast::DUMMY_NODE_ID,
            node: node,
            span: DUMMY_SP,
            attrs: ThinVec::new(),
        })
    }

    struct BracketsFolder {
        failed: bool,
    };
    impl Folder for BracketsFolder {
        fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
            e.map(|e| {
                Expr {
                    node: match e.node {
                        ExprKind::Paren(inner) => {
                            ExprKind::Paren(inner.map(|e| {
                                fold::noop_fold_expr(e, self)
                            }))
                        }
                        ExprKind::If(..) |
                        ExprKind::Block(..) |
                        ExprKind::IfLet(..) => {
                            return fold::noop_fold_expr(e, self);
                        }
                        node => {
                            ExprKind::Paren(expr(node).map(|e| {
                                fold::noop_fold_expr(e, self)
                            }))
                        }
                    },
                    ..e
                }
            })
        }

        fn fold_field(&mut self, f: Field) -> Field {
            Field {
                ident: codemap::respan(f.ident.span, self.fold_ident(f.ident.node)),
                expr: if f.is_shorthand {
                    f.expr.map(|e| fold::noop_fold_expr(e, self))
                } else {
                    self.fold_expr(f.expr)
                },
                span: self.new_span(f.span),
                is_shorthand: f.is_shorthand,
                attrs: fold::fold_thin_attrs(f.attrs, self),
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
                StmtKind::Expr(e) => {
                    StmtKind::Expr(e.map(|e| fold::noop_fold_expr(e, self)))
                }
                StmtKind::Semi(e) => {
                    StmtKind::Semi(e.map(|e| fold::noop_fold_expr(e, self)))
                }
                s => s,
            };

            SmallVector::one(Stmt {
                node: node,
                ..stmt
            })
        }

        fn fold_mac(&mut self, mac: Mac) -> Mac {
            // By default when folding over macros, syntex panics. This is
            // because it's usually not what you want, you want to run after
            // macro expansion. We do want to do that (syn doesn't do macro
            // expansion), so we implement fold_mac to just return the macro
            // unchanged.
            mac
        }
    }

    let mut folder = BracketsFolder {
        failed: false,
    };
    let e = folder.fold_expr(syntex_expr);
    if folder.failed {
        None
    } else {
        Some(e)
    }
}

/// Wrap every expression which is not already wrapped in parens with parens, to
/// reveal the precidence of the parsed expressions, and produce a stringified form
/// of the resulting expression.
fn syn_brackets(syn_expr: syn::Expr) -> syn::Expr {
    use syn::*;
    use syn::fold::*;

    fn paren(folder: &mut BracketsFolder, node: ExprKind) -> ExprKind {
        ExprKind::Paren(ExprParen {
            expr: Box::new(fold_expr(folder, Expr {
                node: node,
                attrs: vec![],
            })),
            paren_token: tokens::Paren::default(),
        })
    }

    struct BracketsFolder;
    impl Folder for BracketsFolder {
        fn fold_expr(&mut self, expr: Expr) -> Expr {
            let kind = match expr.node {
                ExprKind::Group(_) => unreachable!(),
                ExprKind::Paren(p) => paren(self, p.expr.node),
                ExprKind::If(..) |
                ExprKind::Block(..) |
                ExprKind::IfLet(..) => {
                    return fold_expr(self, expr);
                }
                node => paren(self, node),
            };

            Expr {
                node: kind,
                ..expr
            }
        }

        fn fold_stmt(&mut self, stmt: Stmt) -> Stmt {
            match stmt {
                // Don't wrap toplevel expressions in statements.
                Stmt::Expr(e) => {
                    Stmt::Expr(Box::new(fold_expr(self, *e)))
                }
                Stmt::Semi(e, semi) => {
                    Stmt::Semi(Box::new(fold_expr(self, *e)), semi)
                }
                s => s,
            }
        }

        // We don't want to look at expressions that might appear in patterns or
        // types yet. We'll look into comparing those in the future. For now
        // focus on expressions appearing in other places.
        fn fold_pat(&mut self, pat: Pat) -> Pat {
            pat
        }

        fn fold_ty(&mut self, ty: Ty) -> Ty {
            ty
        }
    }

    let mut folder = BracketsFolder;
    folder.fold_expr(syn_expr)
}

/// Walk through a crate collecting all expressions we can find in it.
fn collect_exprs(file: syn::File) -> Vec<syn::Expr> {
    use synom::delimited::Delimited;
    use syn::*;
    use syn::fold::*;

    struct CollectExprsFolder(Vec<Expr>);
    impl Folder for CollectExprsFolder {
        fn fold_expr(&mut self, expr: Expr) -> Expr {
            self.0.push(expr);

            Expr {
                node: ExprKind::Tup(ExprTup {
                    args: Delimited::new(),
                    paren_token: tokens::Paren::default(),
                    lone_comma: None
                }),
                attrs: vec![],
            }
        }
    }

    let mut folder = CollectExprsFolder(vec![]);
    folder.fold_file(file);
    folder.0
}
