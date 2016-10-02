#![cfg(feature = "full")]

#[macro_use]
extern crate quote;
extern crate syn;
extern crate syntex_pos;
extern crate syntex_syntax;
extern crate walkdir;

use syntex_pos::Span;
use syntex_syntax::ast;
use syntex_syntax::parse::{self, ParseSess, PResult};

use std::fs::File;
use std::io::{self, Read, Write};

macro_rules! errorf {
    ($($tt:tt)*) => {
        write!(io::stderr(), $($tt)*).unwrap();
    };
}

#[test]
fn test_round_trip() {
    let mut success = true;

    for entry in walkdir::WalkDir::new("tests/cases").into_iter() {
        let entry = entry.unwrap();

        let path = entry.path();
        if path.extension().map(|e| e != "rs").unwrap_or(true) {
            continue;
        }
        errorf!("=== {}: ", path.display());

        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();

        let krate = match syn::parse_crate(&content) {
            Ok(krate) => krate,
            Err(msg) => {
                errorf!("syn failed to parse\n{}\n", msg);
                success = false;
                continue;
            }
        };
        let back = quote!(#krate).to_string();

        let sess = ParseSess::new();
        let before = syntex_parse(content, &sess).unwrap();
        let after = match syntex_parse(back, &sess) {
            Ok(after) => after,
            Err(mut diagnostic) => {
                errorf!("syntex failed to parse");
                diagnostic.emit();
                success = false;
                continue;
            }
        };

        if before == after {
            errorf!("pass\n");
        } else {
            errorf!("FAIL\nbefore: {:?}\nafter: {:?}\n", before, after);
            success = false;
        }
    }

    assert!(success);
}

fn syntex_parse<'a>(content: String, sess: &'a ParseSess) -> PResult<'a, ast::Crate> {
    let name = "test_round_trip".to_string();
    let cfg = Vec::new();
    parse::parse_crate_from_source_str(name, content, cfg, sess).map(respan_crate)
}

fn respan_crate(krate: ast::Crate) -> ast::Crate {
    use syntex_syntax::ast::{Attribute, Expr, ExprKind, FnDecl, FunctionRetTy, ItemKind, Mac, TyParam};
    use syntex_syntax::codemap::{self, Spanned};
    use syntex_syntax::fold::{self, Folder};
    use syntex_syntax::ptr::P;
    use syntex_syntax::util::move_map::MoveMap;

    struct Respanner;

    impl Respanner {
        fn fold_spanned<T>(&mut self, spanned: Spanned<T>) -> Spanned<T> {
            codemap::respan(self.new_span(spanned.span), spanned.node)
        }
    }

    impl Folder for Respanner {
        fn new_span(&mut self, _: Span) -> Span {
            syntex_pos::DUMMY_SP
        }

        fn fold_item_kind(&mut self, i: ItemKind) -> ItemKind {
            match i {
                ItemKind::Fn(decl, unsafety, constness, abi, generics, body) => {
                    let generics = self.fold_generics(generics);
                    let decl = self.fold_fn_decl(decl);
                    let body = self.fold_block(body);
                    // default fold_item_kind does not fold this span
                    let constness = self.fold_spanned(constness);
                    ItemKind::Fn(decl, unsafety, constness, abi, generics, body)
                }
                _ => fold::noop_fold_item_kind(i, self),
            }
        }

        fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
            e.map(|e| {
                let folded = fold::noop_fold_expr(e, self);
                Expr {
                    node: match folded.node {
                        ExprKind::Lit(l) => ExprKind::Lit(l.map(|l| self.fold_spanned(l))),
                        other => other,
                    },
                    .. folded
                }
            })
        }

        fn fold_ty_param(&mut self, tp: TyParam) -> TyParam {
            TyParam {
                // default fold_ty_param does not fold the span
                span: self.new_span(tp.span),
                .. fold::noop_fold_ty_param(tp, self)
            }
        }

        fn fold_fn_decl(&mut self, decl: P<FnDecl>) -> P<FnDecl> {
            decl.map(|FnDecl {inputs, output, variadic}| FnDecl {
                inputs: inputs.move_map(|x| self.fold_arg(x)),
                output: match output {
                    FunctionRetTy::Ty(ty) => FunctionRetTy::Ty(self.fold_ty(ty)),
                    // default fold_fn_decl does not fold this span
                    FunctionRetTy::Default(span) => FunctionRetTy::Default(self.new_span(span)),
                },
                variadic: variadic
            })
        }

        fn fold_attribute(&mut self, mut at: Attribute) -> Option<Attribute> {
            at.node.id.0 = 0;
            fold::noop_fold_attribute(at, self)
        }

        fn fold_mac(&mut self, mac: Mac) -> Mac {
            fold::noop_fold_mac(mac, self)
        }
    }

    Respanner.fold_crate(krate)
}
