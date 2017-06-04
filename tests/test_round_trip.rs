#![cfg(feature = "full")]

#[macro_use]
extern crate quote;
extern crate syn;
extern crate synom;
extern crate syntex_pos;
extern crate syntex_syntax;
extern crate walkdir;

use syntex_pos::Span;
use syntex_syntax::ast;
use syntex_syntax::parse::{self, ParseSess, PResult};
use syntex_syntax::codemap::FilePathMapping;
use walkdir::{DirEntry, WalkDir, WalkDirIterator};

use std::fs::File;
use std::io::{self, Read, Write};
use std::panic;
use std::time::Instant;

macro_rules! errorf {
    ($($tt:tt)*) => {
        write!(io::stderr(), $($tt)*).unwrap();
    };
}

fn filter(entry: &DirEntry) -> bool {
    let path = entry.path();
    if path.is_dir() {
        return true; // otherwise walkdir does not visit the files
    }
    if path.extension().map(|e| e != "rs").unwrap_or(true) {
        return false;
    }
    let path_string = path.to_string_lossy();
    let path_string = if cfg!(windows) {
        path_string.replace('\\', "/").into()
    } else {
        path_string
    };
    // TODO assert that parsing fails on the parse-fail cases
    if path_string.starts_with("tests/rust/src/test/parse-fail") ||
       path_string.starts_with("tests/rust/src/test/compile-fail") {
        return false;
    }

    if path_string.starts_with("tests/rust/src/test/ui") {
        let stderr_path = path.with_extension("stderr");
        if stderr_path.exists() {
            // Expected to fail in some way
            return false;
        }
    }

    match path_string.as_ref() {
        // TODO better support for attributes
        "tests/rust/src/librustc_data_structures/blake2b.rs" |
        // TODO better support for attributes
        "tests/rust/src/test/incremental/hashes/enum_defs.rs" |
        // TODO better support for attributes
        "tests/rust/src/test/pretty/stmt_expr_attributes.rs" |
        // not actually a test case
        "tests/rust/src/test/run-pass/auxiliary/macro-include-items-expr.rs" |
        // TODO better support for attributes
        "tests/rust/src/test/run-pass/cfg_stmt_expr.rs" |
        // TODO weird glob import
        "tests/rust/src/test/run-pass/import-glob-crate.rs" |
        // TODO better support for attributes
        "tests/rust/src/test/run-pass/inner-attrs-on-impl.rs" |
        // TODO better support for attributes
        "tests/rust/src/test/run-pass/item-attributes.rs" |
        // TODO precedence issue with binop vs poly trait ref
        "tests/rust/src/test/run-pass/try-macro.rs" => false,
        _ => true,
    }
}

/// Abort immediately after this many failures.
fn abort_after() -> u32 {
    if let Ok(s) = std::env::var("ABORT_AFTER_FAILURE") {
        if let Ok(n) = s.parse::<u32>() {
            return n;
        }
    }
    std::u32::MAX
}

#[test]
fn test_round_trip() {
    {
        let min_stack_value = std::env::var("RUST_MIN_STACK").expect("RUST_MIN_STACK env var should be set since some tests require it.");
        let min_stack_value: usize = min_stack_value.parse().expect("RUST_MIN_STACK env var should be set since some tests require it.");
        assert!(min_stack_value >= 16000000);
    }

    let abort_after = abort_after();
    if abort_after == 0 {
        panic!("Skipping all round_trip tests");
    }

    let mut failed = 0;

    let walk = WalkDir::new("tests/rust").sort_by(|a, b| a.cmp(b));
    for entry in walk.into_iter().filter_entry(filter) {
        let entry = entry.unwrap();

        let path = entry.path();
        if path.is_dir() {
            continue;
        }
        errorf!("=== {}: ", path.display());

        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();

        let start = Instant::now();
        let (krate, elapsed) = match content.parse::<syn::Crate>() {
            Ok(krate) => (krate, start.elapsed()),
            Err(msg) => {
                errorf!("syn failed to parse\n{:?}\n", msg);
                failed += 1;
                if failed >= abort_after {
                    panic!("Aborting Immediately due to ABORT_AFTER_FAILURE");
                }
                continue;
            }
        };
        let back = quote!(#krate).to_string();

        let equal = panic::catch_unwind(|| {
            let sess = ParseSess::new(FilePathMapping::empty());
            let before = match syntex_parse(content, &sess) {
                Ok(before) => before,
                Err(mut diagnostic) => {
                    diagnostic.cancel();
                    if diagnostic.message().starts_with("file not found for module") {
                        errorf!("ignore\n");
                    } else {
                        errorf!("ignore - syntex failed to parse original content: {}\n", diagnostic.message());
                    }
                    return true;
                }
            };
            let after = match syntex_parse(back, &sess) {
                Ok(after) => after,
                Err(mut diagnostic) => {
                    errorf!("syntex failed to parse");
                    diagnostic.emit();
                    return false;
                }
            };

            if before == after {
                errorf!("pass in {}ms\n",
                        elapsed.as_secs() * 1000 + elapsed.subsec_nanos() as u64 / 1_000_000);
                true
            } else {
                errorf!("FAIL\nbefore: {}\nafter: {}\n",
                        format!("{:?}", before).replace("\n", ""),
                        format!("{:?}", after).replace("\n", ""));
                false
            }
        });
        match equal {
            Err(_) => errorf!("ignoring syntex panic\n"),
            Ok(true) => {}
            Ok(false) => {
                failed += 1;
                if failed >= abort_after {
                    panic!("Aborting Immediately due to ABORT_AFTER_FAILURE");
                }
            },
        }
    }

    if failed > 0 {
        panic!("{} failures", failed);
    }
}

fn syntex_parse(content: String, sess: &ParseSess) -> PResult<ast::Crate> {
    let name = "test_round_trip".to_string();
    parse::parse_crate_from_source_str(name, content, sess).map(respan_crate)
}

fn respan_crate(krate: ast::Crate) -> ast::Crate {
    use std::rc::Rc;
    use syntex_syntax::ast::{Attribute, Expr, ExprKind, Field, FnDecl, FunctionRetTy, ImplItem,
                             ImplItemKind, ItemKind, Mac, MetaItem, MetaItemKind, MethodSig,
                             NestedMetaItem, NestedMetaItemKind, TraitItem, TraitItemKind, TyParam,
                             Visibility};
    use syntex_syntax::codemap::{self, Spanned};
    use syntex_syntax::fold::{self, Folder};
    use syntex_syntax::parse::token::{Lit, Token};
    use syntex_syntax::ptr::P;
    use syntex_syntax::symbol::Symbol;
    use syntex_syntax::tokenstream::{Delimited, TokenTree};
    use syntex_syntax::util::move_map::MoveMap;
    use syntex_syntax::util::small_vector::SmallVector;

    struct Respanner;

    impl Respanner {
        fn fold_spanned<T>(&mut self, spanned: Spanned<T>) -> Spanned<T> {
            codemap::respan(self.new_span(spanned.span), spanned.node)
        }

        fn fold_lit(&mut self, l: Lit) -> Lit {
            // Give up on comparing literals inside of macros because there are
            // so many equivalent representations of the same literal; they are
            // tested elsewhere
            match l {
                Lit::Byte(_) => Lit::Byte(Symbol::intern("")),
                Lit::Char(_) => Lit::Char(Symbol::intern("")),
                Lit::Integer(_) => Lit::Integer(Symbol::intern("")),
                Lit::Float(_) => Lit::Float(Symbol::intern("")),
                Lit::Str_(_) => Lit::Str_(Symbol::intern("")),
                Lit::ByteStr(_) => Lit::ByteStr(Symbol::intern("")),
                _ => l,
            }
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
                        ExprKind::Lit(l) => {
                            // default fold_expr does not fold lits
                            ExprKind::Lit(l.map(|l| self.fold_spanned(l)))
                        }
                        ExprKind::Binary(op, lhs, rhs) => {
                            // default fold_expr does not fold the op span
                            ExprKind::Binary(self.fold_spanned(op),
                                             self.fold_expr(lhs),
                                             self.fold_expr(rhs))
                        }
                        ExprKind::AssignOp(op, lhs, rhs) => {
                            // default fold_expr does not fold the op span
                            ExprKind::AssignOp(self.fold_spanned(op),
                                               self.fold_expr(lhs),
                                               self.fold_expr(rhs))
                        }
                        other => other,
                    },
                    ..folded
                }
            })
        }

        fn fold_ty_param(&mut self, tp: TyParam) -> TyParam {
            TyParam {
                // default fold_ty_param does not fold the span
                span: self.new_span(tp.span),
                ..fold::noop_fold_ty_param(tp, self)
            }
        }

        fn fold_fn_decl(&mut self, decl: P<FnDecl>) -> P<FnDecl> {
            decl.map(|FnDecl { inputs, output, variadic }| {
                FnDecl {
                    inputs: inputs.move_map(|x| self.fold_arg(x)),
                    output: match output {
                        FunctionRetTy::Ty(ty) => FunctionRetTy::Ty(self.fold_ty(ty)),
                        // default fold_fn_decl does not fold this span
                        FunctionRetTy::Default(span) => FunctionRetTy::Default(self.new_span(span)),
                    },
                    variadic: variadic,
                }
            })
        }

        fn fold_field(&mut self, field: Field) -> Field {
            Field {
                ident: codemap::respan(// default fold_field does not fold this span
                                       self.new_span(field.ident.span),
                                       self.fold_ident(field.ident.node)),
                expr: self.fold_expr(field.expr),
                span: self.new_span(field.span),
                is_shorthand: field.is_shorthand,
                attrs: ast::ThinVec::new(),
            }
        }

        fn fold_trait_item(&mut self, i: TraitItem) -> SmallVector<TraitItem> {
            let noop = fold::noop_fold_trait_item(i, self).expect_one("");
            SmallVector::one(TraitItem {
                                 node: match noop.node {
                                     TraitItemKind::Method(sig, body) => {
                        TraitItemKind::Method(MethodSig {
                                                  constness: self.fold_spanned(sig.constness),
                                                  ..sig
                                              },
                                              body)
                    }
                                     node => node,
                                 },
                                 ..noop
                             })
        }

        fn fold_impl_item(&mut self, i: ImplItem) -> SmallVector<ImplItem> {
            let noop = fold::noop_fold_impl_item(i, self).expect_one("");
            SmallVector::one(ImplItem {
                                 node: match noop.node {
                                     ImplItemKind::Method(sig, body) => {
                        ImplItemKind::Method(MethodSig {
                                                 constness: self.fold_spanned(sig.constness),
                                                 ..sig
                                             },
                                             body)
                    }
                                     node => node,
                                 },
                                 ..noop
                             })
        }

        fn fold_attribute(&mut self, mut at: Attribute) -> Option<Attribute> {
            at.id.0 = 0;
            fold::noop_fold_attribute(at, self)
        }

        fn fold_meta_item(&mut self, meta_item: MetaItem) -> MetaItem {
            let MetaItem { name, node, span } = meta_item;
            MetaItem {
                name: name,
                node: match node {
                    MetaItemKind::Word => MetaItemKind::Word,
                    MetaItemKind::List(nested) => {
                        MetaItemKind::List(nested.move_map(|e| self.fold_meta_list_item(e)))
                    }
                    // default fold_meta_item does not fold the value span
                    MetaItemKind::NameValue(lit) => MetaItemKind::NameValue(self.fold_spanned(lit)),
                },
                span: self.new_span(span),
            }
        }

        fn fold_meta_list_item(&mut self, list_item: NestedMetaItem) -> NestedMetaItem {
            Spanned {
                node: match list_item.node {
                    NestedMetaItemKind::MetaItem(mi) => {
                        NestedMetaItemKind::MetaItem(self.fold_meta_item(mi))
                    }
                    // default fold_meta_list_item does not fold the span
                    NestedMetaItemKind::Literal(lit) => {
                        NestedMetaItemKind::Literal(self.fold_spanned(lit))
                    }
                },
                span: self.new_span(list_item.span),
            }
        }

        fn fold_mac(&mut self, mac: Mac) -> Mac {
            fold::noop_fold_mac(mac, self)
        }

        fn fold_tt(&mut self, tt: TokenTree) -> TokenTree {
            match tt {
                TokenTree::Token(span, ref tok) => {
                    TokenTree::Token(self.new_span(span), self.fold_token(tok.clone()))
                }
                TokenTree::Delimited(span, ref delimed) => {
                    TokenTree::Delimited(self.new_span(span),
                                         Delimited {
                                             delim: delimed.delim,
                                             tts: self.fold_tts(delimed.tts.clone().into()).into(),
                                         })
                }
            }
        }

        fn fold_token(&mut self, t: Token) -> Token {
            match t {
                // default fold_token does not fold literals
                Token::Literal(lit, repr) => Token::Literal(self.fold_lit(lit), repr),
                Token::Ident(id) => Token::Ident(self.fold_ident(id)),
                Token::Lifetime(id) => Token::Lifetime(self.fold_ident(id)),
                Token::Interpolated(nt) => {
                    let nt = match Rc::try_unwrap(nt) {
                        Ok(nt) => nt,
                        Err(nt) => (*nt).clone(),
                    };
                    Token::Interpolated(Rc::new(self.fold_interpolated(nt)))
                }
                Token::SubstNt(ident) => Token::SubstNt(self.fold_ident(ident)),
                _ => t,
            }
        }

        fn fold_vis(&mut self, vis: Visibility) -> Visibility {
            match vis {
                Visibility::Crate(span) => Visibility::Crate(self.new_span(span)),
                _ => fold::noop_fold_vis(vis, self),
            }
        }
    }

    Respanner.fold_crate(krate)
}
