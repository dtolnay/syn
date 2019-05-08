use crate::{file, full, gen};
use inflections::Inflect;
use proc_macro2::{Span, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::*;
use syn_codegen as types;

const FOLD_SRC: &str = "../src/gen/fold.rs";

#[derive(Default)]
struct State {
    fold_trait: TokenStream,
    fold_impl: TokenStream,
}

fn under_name(name: &str) -> Ident {
    Ident::new(&name.to_snake_case(), Span::call_site())
}

fn simple_visit(item: &str, name: &TokenStream) -> TokenStream {
    let ident = under_name(item);

    let method = Ident::new(&format!("fold_{}", ident), Span::call_site());
    quote! {
        _visitor.#method(#name)
    }
}

fn box_visit(
    elem: &types::Type,
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    let res = visit(elem, features, defs, &quote!(*#name))?;
    Some(quote! {
        Box::new(#res)
    })
}

fn vec_visit(
    elem: &types::Type,
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    let operand = quote!(it);
    let val = visit(elem, features, defs, &operand)?;
    Some(quote! {
        FoldHelper::lift(#name, |it| { #val })
    })
}

fn punctuated_visit(
    elem: &types::Type,
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    let operand = quote!(it);
    let val = visit(elem, features, defs, &operand)?;
    Some(quote! {
        FoldHelper::lift(#name, |it| { #val })
    })
}

fn option_visit(
    elem: &types::Type,
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    let it = quote!(it);
    let val = visit(elem, features, defs, &it)?;
    Some(quote! {
        (#name).map(|it| { #val })
    })
}

fn tuple_visit(
    elems: &[types::Type],
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    if elems.is_empty() {
        return None;
    }

    let mut code = TokenStream::new();
    for (i, elem) in elems.iter().enumerate() {
        let i = Index::from(i);
        let it = quote!((#name).#i);
        let val = visit(elem, features, defs, &it).unwrap_or(it);
        code.append_all(val);
        code.append_all(quote!(,));
    }
    Some(quote! {
        (#code)
    })
}

fn token_punct_visit(repr: &str, name: &TokenStream) -> TokenStream {
    let ty: TokenStream = syn::parse_str(&format!("Token![{}]", repr)).unwrap();
    quote! {
        #ty(tokens_helper(_visitor, &#name.spans))
    }
}

fn token_keyword_visit(repr: &str, name: &TokenStream) -> TokenStream {
    let ty: TokenStream = syn::parse_str(&format!("Token![{}]", repr)).unwrap();
    quote! {
        #ty(tokens_helper(_visitor, &#name.span))
    }
}

fn token_group_visit(ty: &str, name: &TokenStream) -> TokenStream {
    let ty = Ident::new(ty, Span::call_site());
    quote! {
        #ty(tokens_helper(_visitor, &#name.span))
    }
}

fn visit(
    ty: &types::Type,
    features: &types::Features,
    defs: &types::Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    match ty {
        types::Type::Box(t) => box_visit(&*t, features, defs, name),
        types::Type::Vec(t) => vec_visit(&*t, features, defs, name),
        types::Type::Punctuated(p) => punctuated_visit(&p.element, features, defs, name),
        types::Type::Option(t) => option_visit(&*t, features, defs, name),
        types::Type::Tuple(t) => tuple_visit(t, features, defs, name),
        types::Type::Token(t) => {
            let repr = &defs.tokens[t];
            let is_keyword = repr.chars().next().unwrap().is_alphabetic();
            if is_keyword {
                Some(token_keyword_visit(repr, name))
            } else {
                Some(token_punct_visit(repr, name))
            }
        }
        types::Type::Group(t) => Some(token_group_visit(&t[..], name)),
        types::Type::Syn(t) => {
            fn requires_full(features: &types::Features) -> bool {
                features.any.contains("full") && features.any.len() == 1
            }

            let res = simple_visit(t, name);

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
        types::Type::Ext(t) if gen::TERMINAL_TYPES.contains(&&t[..]) => Some(simple_visit(t, name)),
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

fn node(state: &mut State, s: &types::Node, defs: &types::Definitions) {
    let features = visit_features(&s.features);
    let under_name = under_name(&s.ident);
    let ty = Ident::new(&s.ident, Span::call_site());
    let fold_fn = Ident::new(&format!("fold_{}", under_name), Span::call_site());

    let mut fold_impl = TokenStream::new();

    match &s.data {
        types::Data::Enum(variants) => {
            let mut fold_variants = TokenStream::new();

            for (variant, fields) in variants {
                let variant_ident = Ident::new(variant, Span::call_site());

                if fields.is_empty() {
                    fold_variants.append_all(quote! {
                        #ty::#variant_ident => {
                            #ty::#variant_ident
                        }
                    });
                } else {
                    let mut bind_fold_fields = TokenStream::new();
                    let mut fold_fields = TokenStream::new();

                    for (idx, ty) in fields.iter().enumerate() {
                        let name = format!("_binding_{}", idx);
                        let binding = Ident::new(&name, Span::call_site());

                        bind_fold_fields.append_all(quote! {
                            #binding,
                        });

                        let owned_binding = quote!(#binding);

                        fold_fields.append_all(
                            visit(ty, &s.features, defs, &owned_binding).unwrap_or(owned_binding),
                        );

                        fold_fields.append_all(quote!(,));
                    }

                    fold_variants.append_all(quote! {
                        #ty::#variant_ident(#bind_fold_fields) => {
                            #ty::#variant_ident(
                                #fold_fields
                            )
                        }
                    });
                }
            }

            fold_impl.append_all(quote! {
                match _i {
                    #fold_variants
                }
            });
        }
        types::Data::Struct(fields) => {
            let mut fold_fields = TokenStream::new();

            for (field, ty) in fields {
                let id = Ident::new(&field, Span::call_site());
                let ref_toks = quote!(_i.#id);
                let fold = visit(&ty, &s.features, defs, &ref_toks).unwrap_or(ref_toks);

                fold_fields.append_all(quote! {
                    #id: #fold,
                });
            }

            if !fields.is_empty() {
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
        types::Data::Private => {
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

    let include_fold_impl = match &s.data {
        types::Data::Private => gen::TERMINAL_TYPES.contains(&s.ident.as_str()),
        types::Data::Struct(_) | types::Data::Enum(_) => true,
    };

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

pub fn generate(defs: &types::Definitions) {
    let state = gen::traverse(defs, node);
    let full_macro = full::get_macro();
    let fold_trait = state.fold_trait;
    let fold_impl = state.fold_impl;
    file::write(
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
}
