//! This crate automatically generates the definition of the `Visit`,
//! `VisitMut`, and `Fold` traits in `syn` based on the `syn` source. It
//! discovers structs and enums declared with the `ast_*` macros and generates
//! the functions for those types.
//!
//! It makes a few assumptions about the target crate:
//! 1. All structs which are discovered must be re-exported in the root of the
//!    crate, even if they were declared in a submodule.
//! 2. This code cannot discover submodules which are located in subdirectories
//!    - only submodules located in the same directory.
//! 3. The path to `syn` is hardcoded.

use crate::types;
use indexmap::IndexMap;
use proc_macro2::TokenStream;

use std::fs::File;
use std::io::Write;

const FOLD_SRC: &str = "../src/gen/fold.rs";
const VISIT_SRC: &str = "../src/gen/visit.rs";
const VISIT_MUT_SRC: &str = "../src/gen/visit_mut.rs";

mod codegen {
    use crate::types;
    use inflections::Inflect;
    use proc_macro2::{Span, TokenStream};
    use quote::TokenStreamExt;
    use syn::*;

    #[derive(Default)]
    pub struct State {
        pub visit_trait: TokenStream,
        pub visit_impl: TokenStream,
        pub visit_mut_trait: TokenStream,
        pub visit_mut_impl: TokenStream,
        pub fold_trait: TokenStream,
        pub fold_impl: TokenStream,
    }

    fn under_name(name: &str) -> Ident {
        Ident::new(&name.to_snake_case(), Span::call_site())
    }

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    enum Kind {
        Visit,
        VisitMut,
        Fold,
    }

    enum Operand {
        Borrowed(TokenStream),
        Owned(TokenStream),
    }

    use self::Kind::*;
    use self::Operand::*;

    impl Operand {
        fn tokens(&self) -> &TokenStream {
            match *self {
                Borrowed(ref n) | Owned(ref n) => n,
            }
        }

        fn ref_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => n.clone(),
                Owned(ref n) => quote!(&#n),
            }
        }

        fn ref_mut_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => n.clone(),
                Owned(ref n) => quote!(&mut #n),
            }
        }

        fn owned_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => quote!(*#n),
                Owned(ref n) => n.clone(),
            }
        }
    }

    fn simple_visit(item: &str, kind: Kind, name: &Operand) -> TokenStream {
        let ident = under_name(item);

        match kind {
            Visit => {
                let method = Ident::new(&format!("visit_{}", ident), Span::call_site());
                let name = name.ref_tokens();
                quote! {
                    _visitor.#method(#name)
                }
            }
            VisitMut => {
                let method = Ident::new(&format!("visit_{}_mut", ident), Span::call_site());
                let name = name.ref_mut_tokens();
                quote! {
                    _visitor.#method(#name)
                }
            }
            Fold => {
                let method = Ident::new(&format!("fold_{}", ident), Span::call_site());
                let name = name.owned_tokens();
                quote! {
                    _visitor.#method(#name)
                }
            }
        }
    }

    fn box_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let name = name.owned_tokens();
        let res = visit(elem, features, defs, kind, &Owned(quote!(*#name)))?;
        Some(match kind {
            Fold => quote! {
                Box::new(#res)
            },
            Visit | VisitMut => res,
        })
    }

    fn vec_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let operand = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, features, defs, kind, &operand)?;
        Some(match kind {
            Visit => {
                let name = name.ref_tokens();
                quote! {
                    for it in #name {
                        #val
                    }
                }
            }
            VisitMut => {
                let name = name.ref_mut_tokens();
                quote! {
                    for it in #name {
                        #val
                    }
                }
            }
            Fold => {
                let name = name.owned_tokens();
                quote! {
                    FoldHelper::lift(#name, |it| { #val })
                }
            }
        })
    }

    fn punctuated_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let operand = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, features, defs, kind, &operand)?;
        Some(match kind {
            Visit => {
                let name = name.ref_tokens();
                quote! {
                    for el in Punctuated::pairs(#name) {
                        let it = el.value();
                        #val
                    }
                }
            }
            VisitMut => {
                let name = name.ref_mut_tokens();
                quote! {
                    for mut el in Punctuated::pairs_mut(#name) {
                        let it = el.value_mut();
                        #val
                    }
                }
            }
            Fold => {
                let name = name.owned_tokens();
                quote! {
                    FoldHelper::lift(#name, |it| { #val })
                }
            }
        })
    }

    fn option_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let it = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, features, defs, kind, &it)?;
        let name = name.owned_tokens();
        Some(match kind {
            Visit => quote! {
                if let Some(ref it) = #name {
                    #val
                }
            },
            VisitMut => quote! {
                if let Some(ref mut it) = #name {
                    #val
                }
            },
            Fold => quote! {
                (#name).map(|it| { #val })
            },
        })
    }

    fn tuple_visit(
        elems: &[types::Type],
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        if elems.is_empty() {
            return None;
        }

        let mut code = TokenStream::new();
        for (i, elem) in elems.iter().enumerate() {
            let name = name.tokens();
            let i = Index::from(i);
            let it = Owned(quote!((#name).#i));
            let val =
                visit(elem, features, defs, kind, &it).unwrap_or_else(|| noop_visit(kind, &it));
            code.append_all(val);
            match kind {
                Fold => code.append_all(quote!(,)),
                Visit | VisitMut => code.append_all(quote!(;)),
            }
        }
        Some(match kind {
            Fold => quote! {
                (#code)
            },
            Visit | VisitMut => code,
        })
    }

    fn token_punct_visit(repr: &str, kind: Kind, name: &Operand) -> TokenStream {
        let ty: TokenStream = syn::parse_str(&format!("Token![{}]", repr)).unwrap();
        let name = name.tokens();
        match kind {
            Fold => quote! {
                #ty(tokens_helper(_visitor, &#name.spans))
            },
            Visit => quote! {
                tokens_helper(_visitor, &#name.spans)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.spans)
            },
        }
    }

    fn token_keyword_visit(repr: &str, kind: Kind, name: &Operand) -> TokenStream {
        let ty: TokenStream = syn::parse_str(&format!("Token![{}]", repr)).unwrap();
        let name = name.tokens();
        match kind {
            Fold => quote! {
                #ty(tokens_helper(_visitor, &#name.span))
            },
            Visit => quote! {
                tokens_helper(_visitor, &#name.span)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.span)
            },
        }
    }

    fn token_group_visit(ty: &str, kind: Kind, name: &Operand) -> TokenStream {
        let ty = Ident::new(ty, Span::call_site());
        let name = name.tokens();
        match kind {
            Fold => quote! {
                #ty(tokens_helper(_visitor, &#name.span))
            },
            Visit => quote! {
                tokens_helper(_visitor, &#name.span)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.span)
            },
        }
    }

    fn noop_visit(kind: Kind, name: &Operand) -> TokenStream {
        match kind {
            Fold => name.owned_tokens(),
            Visit | VisitMut => {
                let name = name.tokens();
                quote! {
                    skip!(#name)
                }
            }
        }
    }

    fn visit(
        ty: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        match ty {
            types::Type::Box(t) => box_visit(&*t, features, defs, kind, name),
            types::Type::Vec(t) => vec_visit(&*t, features, defs, kind, name),
            types::Type::Punctuated(p) => punctuated_visit(p.element(), features, defs, kind, name),
            types::Type::Option(t) => option_visit(&*t, features, defs, kind, name),
            types::Type::Tuple(t) => tuple_visit(t, features, defs, kind, name),
            types::Type::Token(t) => {
                let repr = &defs.tokens[t];
                let is_keyword = repr.chars().next().unwrap().is_alphabetic();
                if is_keyword {
                    Some(token_keyword_visit(repr, kind, name))
                } else {
                    Some(token_punct_visit(repr, kind, name))
                }
            }
            types::Type::Group(t) => Some(token_group_visit(&t[..], kind, name)),
            types::Type::Syn(t) => {
                fn requires_full(features: &types::Features) -> bool {
                    features.contains("full") && features.len() == 1
                }

                let mut res = simple_visit(t, kind, name);

                let target = defs.types.iter().find(|ty| ty.ident() == t).unwrap();

                Some(
                    if requires_full(target.features()) && !requires_full(features) {
                        quote! {
                            full!(#res)
                        }
                    } else {
                        res
                    },
                )
            }
            types::Type::Ext(t) if super::TERMINAL_TYPES.contains(&&t[..]) => {
                Some(simple_visit(t, kind, name))
            }
            types::Type::Ext(_) | types::Type::Std(_) => None,
        }
    }

    fn visit_features(features: &types::Features) -> TokenStream {
        match features.len() {
            0 => quote!(),
            1 => {
                let feature = &features[0];
                quote!(#[cfg(feature = #feature)])
            }
            _ => {
                let features = features.iter().map(|feature| quote!(feature = #feature));

                quote!(#[cfg(any( #(#features),* ))])
            }
        }
    }

    pub fn generate(state: &mut State, s: &types::Node, defs: &types::Definitions) {
        let features = visit_features(s.features());
        let under_name = under_name(s.ident());
        let ty = Ident::new(s.ident(), Span::call_site());
        let visit_fn = Ident::new(&format!("visit_{}", under_name), Span::call_site());
        let visit_mut_fn = Ident::new(&format!("visit_{}_mut", under_name), Span::call_site());
        let fold_fn = Ident::new(&format!("fold_{}", under_name), Span::call_site());

        let mut visit_impl = TokenStream::new();
        let mut visit_mut_impl = TokenStream::new();
        let mut fold_impl = TokenStream::new();

        match s {
            types::Node::Enum(ref e) => {
                let mut visit_variants = TokenStream::new();
                let mut visit_mut_variants = TokenStream::new();
                let mut fold_variants = TokenStream::new();

                for variant in e.variants() {
                    let variant_ident = Ident::new(variant.ident(), Span::call_site());

                    if variant.fields().is_empty() {
                        visit_variants.append_all(quote! {
                            #ty::#variant_ident => {}
                        });
                        visit_mut_variants.append_all(quote! {
                            #ty::#variant_ident => {}
                        });
                        fold_variants.append_all(quote! {
                            #ty::#variant_ident => {
                                #ty::#variant_ident
                            }
                        });
                    } else {
                        let mut bind_visit_fields = TokenStream::new();
                        let mut bind_visit_mut_fields = TokenStream::new();
                        let mut bind_fold_fields = TokenStream::new();

                        let mut visit_fields = TokenStream::new();
                        let mut visit_mut_fields = TokenStream::new();
                        let mut fold_fields = TokenStream::new();

                        for (idx, ty) in variant.fields().iter().enumerate() {
                            let name = format!("_binding_{}", idx);
                            let binding = Ident::new(&name, Span::call_site());

                            bind_visit_fields.append_all(quote! {
                                ref #binding,
                            });
                            bind_visit_mut_fields.append_all(quote! {
                                ref mut #binding,
                            });
                            bind_fold_fields.append_all(quote! {
                                #binding,
                            });

                            let borrowed_binding = Borrowed(quote!(#binding));
                            let owned_binding = Owned(quote!(#binding));

                            visit_fields.append_all(
                                visit(ty, s.features(), defs, Visit, &borrowed_binding)
                                    .unwrap_or_else(|| noop_visit(Visit, &borrowed_binding)),
                            );
                            visit_mut_fields.append_all(
                                visit(ty, s.features(), defs, VisitMut, &borrowed_binding)
                                    .unwrap_or_else(|| noop_visit(VisitMut, &borrowed_binding)),
                            );
                            fold_fields.append_all(
                                visit(ty, s.features(), defs, Fold, &owned_binding)
                                    .unwrap_or_else(|| noop_visit(Fold, &owned_binding)),
                            );

                            visit_fields.append_all(quote!(;));
                            visit_mut_fields.append_all(quote!(;));
                            fold_fields.append_all(quote!(,));
                        }

                        visit_variants.append_all(quote! {
                            #ty::#variant_ident(#bind_visit_fields) => {
                                #visit_fields
                            }
                        });

                        visit_mut_variants.append_all(quote! {
                            #ty::#variant_ident(#bind_visit_mut_fields) => {
                                #visit_mut_fields
                            }
                        });

                        fold_variants.append_all(quote! {
                            #ty::#variant_ident(#bind_fold_fields) => {
                                #ty::#variant_ident(
                                    #fold_fields
                                )
                            }
                        });
                    }
                }

                visit_impl.append_all(quote! {
                    match *_i {
                        #visit_variants
                    }
                });

                visit_mut_impl.append_all(quote! {
                    match *_i {
                        #visit_mut_variants
                    }
                });

                fold_impl.append_all(quote! {
                    match _i {
                        #fold_variants
                    }
                });
            }
            types::Node::Struct(ref v) => {
                let mut fold_fields = TokenStream::new();

                for (field, ty) in v.fields() {
                    let id = Ident::new(field, Span::call_site());
                    let ref_toks = Owned(quote!(_i.#id));
                    let visit_field = visit(ty, v.features(), defs, Visit, &ref_toks)
                        .unwrap_or_else(|| noop_visit(Visit, &ref_toks));
                    visit_impl.append_all(quote! {
                        #visit_field;
                    });
                    let visit_mut_field = visit(ty, v.features(), defs, VisitMut, &ref_toks)
                        .unwrap_or_else(|| noop_visit(VisitMut, &ref_toks));
                    visit_mut_impl.append_all(quote! {
                        #visit_mut_field;
                    });
                    let fold = visit(ty, v.features(), defs, Fold, &ref_toks)
                        .unwrap_or_else(|| noop_visit(Fold, &ref_toks));

                    fold_fields.append_all(quote! {
                        #id: #fold,
                    });
                }

                if !v.fields().is_empty() {
                    fold_impl.append_all(quote! {
                        #ty {
                            #fold_fields
                        }
                    })
                } else {
                    if ty == "Ident" {
                        fold_impl.append_all(quote! {
                            let mut _i = _i;
                            let span = _visitor.fold_span(_i.span());
                            _i.set_span(span);
                        });
                    }
                    fold_impl.append_all(quote! {
                        _i
                    });
                }
            }
        }

        let mut include_fold_impl = true;
        if let types::Node::Struct(ref data) = s {
            if data.fields().is_empty() && !super::TERMINAL_TYPES.contains(&&s.ident()) {
                include_fold_impl = false;
            }
        }

        state.visit_trait.append_all(quote! {
            #features
            fn #visit_fn(&mut self, i: &'ast #ty) {
                #visit_fn(self, i)
            }
        });

        state.visit_impl.append_all(quote! {
            #features
            pub fn #visit_fn<'ast, V: Visit<'ast> + ?Sized>(
                _visitor: &mut V, _i: &'ast #ty
            ) {
                #visit_impl
            }
        });

        state.visit_mut_trait.append_all(quote! {
            #features
            fn #visit_mut_fn(&mut self, i: &mut #ty) {
                #visit_mut_fn(self, i)
            }
        });

        state.visit_mut_impl.append_all(quote! {
            #features
            pub fn #visit_mut_fn<V: VisitMut + ?Sized>(
                _visitor: &mut V, _i: &mut #ty
            ) {
                #visit_mut_impl
            }
        });

        state.fold_trait.append_all(quote! {
            #features
            fn #fold_fn(&mut self, i: #ty) -> #ty {
                #fold_fn(self, i)
            }
        });

        if include_fold_impl {
            state.fold_impl.append_all(quote! {
                #features
                pub fn #fold_fn<V: Fold + ?Sized>(
                    _visitor: &mut V, _i: #ty
                ) -> #ty {
                    #fold_impl
                }
            });
        }
    }
}

fn write_file(path: &str, content: TokenStream) {
    let mut file = File::create(path).unwrap();
    write!(
        file,
        "// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT\n\n"
    )
    .unwrap();
    let mut config = rustfmt::Config::default();
    config.set().emit_mode(rustfmt::EmitMode::Stdout);
    config.set().verbose(rustfmt::Verbosity::Quiet);
    config.set().format_macro_matchers(true);
    config.set().normalize_doc_attributes(true);
    let mut session = rustfmt::Session::new(config, Some(&mut file));
    session
        .format(rustfmt::Input::Text(content.to_string()))
        .unwrap();
}

const TERMINAL_TYPES: &[&str] = &["Span", "Ident"];

pub fn generate(defs: &types::Definitions) {
    let mut defs = defs.clone();

    for &tt in TERMINAL_TYPES {
        defs.insert(types::Node::Struct(types::Struct::new(
            tt.to_string(),
            types::Features::default(),
            IndexMap::new(),
        )));
    }

    let mut state = codegen::State::default();
    for s in &defs.types {
        codegen::generate(&mut state, s, &defs);
    }

    let full_macro = quote! {
        #[cfg(feature = "full")]
        macro_rules! full {
            ($e:expr) => {
                $e
            };
        }

        #[cfg(all(feature = "derive", not(feature = "full")))]
        macro_rules! full {
            ($e:expr) => {
                unreachable!()
            };
        }
    };

    let skip_macro = quote! {
        #[cfg(any(feature = "full", feature = "derive"))]
        macro_rules! skip {
            ($($tt:tt)*) => {};
        }
    };

    let fold_trait = state.fold_trait;
    let fold_impl = state.fold_impl;
    write_file(
        FOLD_SRC,
        quote! {
            // Unreachable code is generated sometimes without the full feature.
            #![allow(unreachable_code)]

            use *;
            #[cfg(any(feature = "full", feature = "derive"))]
            use token::{Brace, Bracket, Paren, Group};
            use proc_macro2::Span;
            #[cfg(any(feature = "full", feature = "derive"))]
            use gen::helper::fold::*;

            #full_macro

            /// Syntax tree traversal to transform the nodes of an owned syntax tree.
            ///
            /// See the [module documentation] for details.
            ///
            /// [module documentation]: index.html
            ///
            /// *This trait is available if Syn is built with the `"fold"` feature.*
            pub trait Fold {
                #fold_trait
            }

            #[cfg(any(feature = "full", feature = "derive"))]
            macro_rules! fold_span_only {
                ($f:ident : $t:ident) => {
                    pub fn $f<V: Fold + ?Sized>(_visitor: &mut V, mut _i: $t) -> $t {
                        let span = _visitor.fold_span(_i.span());
                        _i.set_span(span);
                        _i
                    }
                }
            }

            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_byte: LitByte);
            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_byte_str: LitByteStr);
            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_char: LitChar);
            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_float: LitFloat);
            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_int: LitInt);
            #[cfg(any(feature = "full", feature = "derive"))]
            fold_span_only!(fold_lit_str: LitStr);

            #fold_impl
        },
    );

    let visit_trait = state.visit_trait;
    let visit_impl = state.visit_impl;
    write_file(
        VISIT_SRC,
        quote! {
            #![cfg_attr(feature = "cargo-clippy", allow(trivially_copy_pass_by_ref))]

            use *;
            #[cfg(any(feature = "full", feature = "derive"))]
            use punctuated::Punctuated;
            use proc_macro2::Span;
            #[cfg(any(feature = "full", feature = "derive"))]
            use gen::helper::visit::*;

            #full_macro
            #skip_macro

            /// Syntax tree traversal to walk a shared borrow of a syntax tree.
            ///
            /// See the [module documentation] for details.
            ///
            /// [module documentation]: index.html
            ///
            /// *This trait is available if Syn is built with the `"visit"` feature.*
            pub trait Visit<'ast> {
                #visit_trait
            }

            #visit_impl
        },
    );

    let visit_mut_trait = state.visit_mut_trait;
    let visit_mut_impl = state.visit_mut_impl;
    write_file(
        VISIT_MUT_SRC,
        quote! {
            use *;
            #[cfg(any(feature = "full", feature = "derive"))]
            use punctuated::Punctuated;
            use proc_macro2::Span;
            #[cfg(any(feature = "full", feature = "derive"))]
            use gen::helper::visit_mut::*;

            #full_macro
            #skip_macro

            /// Syntax tree traversal to mutate an exclusive borrow of a syntax tree in
            /// place.
            ///
            /// See the [module documentation] for details.
            ///
            /// [module documentation]: index.html
            ///
            /// *This trait is available if Syn is built with the `"visit-mut"` feature.*
            pub trait VisitMut {
                #visit_mut_trait
            }

            #visit_mut_impl
        },
    );
}
