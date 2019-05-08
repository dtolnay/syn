use crate::file;
use quote::quote;
use syn_codegen as types;

const VISIT_SRC: &str = "../src/gen/visit.rs";
const VISIT_MUT_SRC: &str = "../src/gen/visit_mut.rs";

mod codegen {
    use inflections::Inflect;
    use proc_macro2::{Span, TokenStream};
    use quote::{quote, TokenStreamExt};
    use syn::*;
    use syn_codegen as types;

    #[derive(Default)]
    pub struct State {
        pub visit_trait: TokenStream,
        pub visit_impl: TokenStream,
        pub visit_mut_trait: TokenStream,
        pub visit_mut_impl: TokenStream,
    }

    fn under_name(name: &str) -> Ident {
        Ident::new(&name.to_snake_case(), Span::call_site())
    }

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    enum Kind {
        Visit,
        VisitMut,
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
        Some(res)
    }

    fn vec_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let operand = Borrowed(quote!(it));
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
        })
    }

    fn punctuated_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let operand = Borrowed(quote!(it));
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
        })
    }

    fn option_visit(
        elem: &types::Type,
        features: &types::Features,
        defs: &types::Definitions,
        kind: Kind,
        name: &Operand,
    ) -> Option<TokenStream> {
        let it = Borrowed(quote!(it));
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
            let val = visit(elem, features, defs, kind, &it).unwrap_or_else(|| noop_visit(&it));
            code.append_all(val);
            code.append_all(quote!(;));
        }
        Some(code)
    }

    fn token_punct_visit(kind: Kind, name: &Operand) -> TokenStream {
        let name = name.tokens();
        match kind {
            Visit => quote! {
                tokens_helper(_visitor, &#name.spans)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.spans)
            },
        }
    }

    fn token_keyword_visit(kind: Kind, name: &Operand) -> TokenStream {
        let name = name.tokens();
        match kind {
            Visit => quote! {
                tokens_helper(_visitor, &#name.span)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.span)
            },
        }
    }

    fn token_group_visit(kind: Kind, name: &Operand) -> TokenStream {
        let name = name.tokens();
        match kind {
            Visit => quote! {
                tokens_helper(_visitor, &#name.span)
            },
            VisitMut => quote! {
                tokens_helper(_visitor, &mut #name.span)
            },
        }
    }

    fn noop_visit(name: &Operand) -> TokenStream {
        let name = name.tokens();
        quote! {
            skip!(#name)
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
            types::Type::Punctuated(p) => punctuated_visit(&p.element, features, defs, kind, name),
            types::Type::Option(t) => option_visit(&*t, features, defs, kind, name),
            types::Type::Tuple(t) => tuple_visit(t, features, defs, kind, name),
            types::Type::Token(t) => {
                let repr = &defs.tokens[t];
                let is_keyword = repr.chars().next().unwrap().is_alphabetic();
                if is_keyword {
                    Some(token_keyword_visit(kind, name))
                } else {
                    Some(token_punct_visit(kind, name))
                }
            }
            types::Type::Group(_) => Some(token_group_visit(kind, name)),
            types::Type::Syn(t) => {
                fn requires_full(features: &types::Features) -> bool {
                    features.any.contains("full") && features.any.len() == 1
                }

                let res = simple_visit(t, kind, name);

                let target = defs.types.iter().find(|ty| ty.ident == *t).unwrap();

                Some(
                    if requires_full(&target.features) && !requires_full(features) {
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
        let features = &features.any;
        match features.len() {
            0 => quote!(),
            1 => quote!(#[cfg(feature = #(#features)*)]),
            _ => quote!(#[cfg(any(#(feature = #features),*))]),
        }
    }

    pub fn generate(state: &mut State, s: &types::Node, defs: &types::Definitions) {
        let features = visit_features(&s.features);
        let under_name = under_name(&s.ident);
        let ty = Ident::new(&s.ident, Span::call_site());
        let visit_fn = Ident::new(&format!("visit_{}", under_name), Span::call_site());
        let visit_mut_fn = Ident::new(&format!("visit_{}_mut", under_name), Span::call_site());

        let mut visit_impl = TokenStream::new();
        let mut visit_mut_impl = TokenStream::new();

        match &s.data {
            types::Data::Enum(variants) => {
                let mut visit_variants = TokenStream::new();
                let mut visit_mut_variants = TokenStream::new();

                for (variant, fields) in variants {
                    let variant_ident = Ident::new(variant, Span::call_site());

                    if fields.is_empty() {
                        visit_variants.append_all(quote! {
                            #ty::#variant_ident => {}
                        });
                        visit_mut_variants.append_all(quote! {
                            #ty::#variant_ident => {}
                        });
                    } else {
                        let mut bind_visit_fields = TokenStream::new();
                        let mut bind_visit_mut_fields = TokenStream::new();

                        let mut visit_fields = TokenStream::new();
                        let mut visit_mut_fields = TokenStream::new();

                        for (idx, ty) in fields.iter().enumerate() {
                            let name = format!("_binding_{}", idx);
                            let binding = Ident::new(&name, Span::call_site());

                            bind_visit_fields.append_all(quote! {
                                ref #binding,
                            });
                            bind_visit_mut_fields.append_all(quote! {
                                ref mut #binding,
                            });

                            let borrowed_binding = Borrowed(quote!(#binding));

                            visit_fields.append_all(
                                visit(ty, &s.features, defs, Visit, &borrowed_binding)
                                    .unwrap_or_else(|| noop_visit(&borrowed_binding)),
                            );
                            visit_mut_fields.append_all(
                                visit(ty, &s.features, defs, VisitMut, &borrowed_binding)
                                    .unwrap_or_else(|| noop_visit(&borrowed_binding)),
                            );

                            visit_fields.append_all(quote!(;));
                            visit_mut_fields.append_all(quote!(;));
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
            }
            types::Data::Struct(fields) => {
                for (field, ty) in fields {
                    let id = Ident::new(&field, Span::call_site());
                    let ref_toks = Owned(quote!(_i.#id));
                    let visit_field = visit(&ty, &s.features, defs, Visit, &ref_toks)
                        .unwrap_or_else(|| noop_visit(&ref_toks));
                    visit_impl.append_all(quote! {
                        #visit_field;
                    });
                    let visit_mut_field = visit(&ty, &s.features, defs, VisitMut, &ref_toks)
                        .unwrap_or_else(|| noop_visit(&ref_toks));
                    visit_mut_impl.append_all(quote! {
                        #visit_mut_field;
                    });
                }
            }
            types::Data::Private => {}
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
    }
}

const TERMINAL_TYPES: &[&str] = &["Span", "Ident"];

pub fn generate(defs: &types::Definitions) {
    let mut state = codegen::State::default();
    for s in &defs.types {
        codegen::generate(&mut state, s, defs);
    }
    for tt in TERMINAL_TYPES {
        let s = types::Node {
            ident: tt.to_string(),
            features: types::Features::default(),
            data: types::Data::Private,
        };
        codegen::generate(&mut state, &s, defs);
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

    let visit_trait = state.visit_trait;
    let visit_impl = state.visit_impl;
    file::write(
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
    file::write(
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
