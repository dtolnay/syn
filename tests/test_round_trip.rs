#![cfg(feature = "full")]

#[macro_use]
extern crate quote;
extern crate syn;
extern crate syntex_pos;
extern crate syntex_syntax;
extern crate time;
extern crate walkdir;

use syntex_pos::Span;
use syntex_syntax::ast;
use syntex_syntax::parse::{self, ParseSess, PResult};
use time::PreciseTime;


use std::fs::File;
use std::io::{self, Read, Write};

macro_rules! errorf {
    ($($tt:tt)*) => {
        write!(io::stderr(), $($tt)*).unwrap();
    };
}

#[test]
fn test_round_trip() {
    let mut failed = 0;

    let walk = walkdir::WalkDir::new("tests/cases").sort_by(|a, b| a.cmp(b));
    for entry in walk.into_iter() {
        let entry = entry.unwrap();

        let path = entry.path();
        if path.extension().map(|e| e != "rs").unwrap_or(true) {
            continue;
        }
        errorf!("=== {}: ", path.display());

        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();

        let start = PreciseTime::now();
        let (krate, elapsed) = match syn::parse_crate(&content) {
            Ok(krate) => (krate, start.to(PreciseTime::now())),
            Err(msg) => {
                errorf!("syn failed to parse\n{}\n", msg);
                failed += 1;
                continue;
            }
        };
        let back = quote!(#krate).to_string();

        let sess = ParseSess::new();
        let before = match syntex_parse(content, &sess) {
            Ok(before) => before,
            Err(mut diagnostic) => {
                diagnostic.cancel();
                if diagnostic.message.starts_with("file not found for module") ||
                   diagnostic.message.starts_with("couldn't read") {
                    errorf!("ignore\n");
                    continue;
                } else {
                    panic!(diagnostic.message.clone());
                }
            }
        };
        let after = match syntex_parse(back, &sess) {
            Ok(after) => after,
            Err(mut diagnostic) => {
                errorf!("syntex failed to parse");
                diagnostic.emit();
                failed += 1;
                continue;
            }
        };

        if before == after {
            errorf!("pass in {}ms\n", elapsed.num_milliseconds());
        } else {
            errorf!("FAIL\nbefore: {:?}\nafter: {:?}\n", before, after);
            failed += 1;
        }
    }

    if failed > 0 {
        panic!("{} failures", failed);
    }
}

fn syntex_parse<'a>(content: String, sess: &'a ParseSess) -> PResult<'a, ast::Crate> {
    let name = "test_round_trip".to_string();
    let cfg = Vec::new();
    parse::parse_crate_from_source_str(name, content, cfg, sess).map(respan_crate)
}

fn respan_crate(krate: ast::Crate) -> ast::Crate {
    use std::rc::Rc;
    use syntex_syntax::ast::{Attribute, Expr, ExprKind, Field, FnDecl, FunctionRetTy, ImplItem,
                             ImplItemKind, ItemKind, Mac, MetaItem, MetaItemKind, MethodSig,
                             NestedMetaItem, NestedMetaItemKind, TraitItem, TraitItemKind, TyParam};
    use syntex_syntax::codemap::{self, Spanned};
    use syntex_syntax::fold::{self, Folder};
    use syntex_syntax::parse::token::{Lit, Token, intern};
    use syntex_syntax::ptr::P;
    use syntex_syntax::tokenstream::{Delimited, SequenceRepetition, TokenTree};
    use syntex_syntax::util::move_map::MoveMap;
    use syntex_syntax::util::small_vector::SmallVector;

    struct Respanner;

    impl Respanner {
        fn fold_spanned<T>(&mut self, spanned: Spanned<T>) -> Spanned<T> {
            codemap::respan(self.new_span(spanned.span), spanned.node)
        }

        fn fold_lit(&mut self, l: Lit) -> Lit {
            match l {
                Lit::Integer(repr) => Lit::Integer(intern(&repr.to_string().replace("_", ""))),
                Lit::Str_(_) => {
                    // Give up on comparing strings because there are so many
                    // equivalent representations of the same string; they are
                    // tested elsewhere
                    Lit::Str_(intern(""))
                }
                Lit::Float(repr) => Lit::Float(intern(&repr.to_string().replace("_", ""))),
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
            at.node.id.0 = 0;
            fold::noop_fold_attribute(at, self)
        }

        fn fold_meta_item(&mut self, meta_item: P<MetaItem>) -> P<MetaItem> {
            meta_item.map(|Spanned { node, span }| {
                Spanned {
                    node: match node {
                        MetaItemKind::Word(id) => MetaItemKind::Word(id),
                        MetaItemKind::List(id, mis) => {
                            MetaItemKind::List(id, mis.move_map(|e| self.fold_meta_list_item(e)))
                        }
                        // default fold_meta_item does not fold the value span
                        MetaItemKind::NameValue(id, lit) => {
                            MetaItemKind::NameValue(id, self.fold_spanned(lit))
                        }
                    },
                    span: self.new_span(span),
                }
            })
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

        fn fold_tt(&mut self, tt: &TokenTree) -> TokenTree {
            match *tt {
                TokenTree::Token(span, ref tok) => {
                    TokenTree::Token(self.new_span(span), self.fold_token(tok.clone()))
                }
                TokenTree::Delimited(span, ref delimed) => {
                    TokenTree::Delimited(self.new_span(span),
                                         Rc::new(Delimited {
                                             delim: delimed.delim,
                                             open_span: self.new_span(delimed.open_span),
                                             tts: self.fold_tts(&delimed.tts),
                                             close_span: self.new_span(delimed.close_span),
                                         }))
                }
                TokenTree::Sequence(span, ref seq) => {
                    TokenTree::Sequence(self.new_span(span),
                                        Rc::new(SequenceRepetition {
                                            tts: self.fold_tts(&seq.tts),
                                            separator: seq.separator
                                                .clone()
                                                .map(|tok| self.fold_token(tok)),
                                            ..**seq
                                        }))
                }
            }
        }

        fn fold_token(&mut self, t: Token) -> Token {
            match t {
                // default fold_token does not fold literals
                Token::Literal(lit, repr) => Token::Literal(self.fold_lit(lit), repr),
                Token::Ident(id) => Token::Ident(self.fold_ident(id)),
                Token::Lifetime(id) => Token::Lifetime(self.fold_ident(id)),
                Token::Interpolated(nt) => Token::Interpolated(self.fold_interpolated(nt)),
                Token::SubstNt(ident) => Token::SubstNt(self.fold_ident(ident)),
                Token::MatchNt(name, kind) => {
                    Token::MatchNt(self.fold_ident(name), self.fold_ident(kind))
                }
                _ => t,
            }
        }
    }

    Respanner.fold_crate(krate)
}
