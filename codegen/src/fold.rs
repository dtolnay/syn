use crate::{file, full, r#gen};
use anyhow::Result;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::Index;
use syn_codegen::{Data, Definitions, Features, Node, Type};

const FOLD_SRC: &str = "src/gen/fold.rs";

fn simple_visit(item: &str, name: &TokenStream) -> TokenStream {
    let ident = r#gen::under_name(item);
    let method = format_ident!("fold_{}", ident);
    quote! {
        f.#method(#name)
    }
}

fn visit(
    ty: &Type,
    features: &Features,
    defs: &Definitions,
    name: &TokenStream,
) -> Option<TokenStream> {
    match ty {
        Type::Box(t) => {
            let res = visit(t, features, defs, &quote!(*#name))?;
            Some(quote! {
                Box::new(#res)
            })
        }
        Type::Vec(t) => {
            let operand = quote!(it);
            let val = visit(t, features, defs, &operand)?;
            Some(quote! {
                FoldHelper::lift(#name, |it| #val)
            })
        }
        Type::Punctuated(p) => {
            let operand = quote!(it);
            let val = visit(&p.element, features, defs, &operand)?;
            Some(quote! {
                FoldHelper::lift(#name, |it| #val)
            })
        }
        Type::Option(t) => {
            let it = quote!(it);
            let val = visit(t, features, defs, &it)?;
            Some(quote! {
                (#name).map(|it| #val)
            })
        }
        Type::Tuple(t) => {
            let mut code = TokenStream::new();
            for (i, elem) in t.iter().enumerate() {
                let i = Index::from(i);
                let it = quote!((#name).#i);
                let val = visit(elem, features, defs, &it).unwrap_or(it);
                code.extend(val);
                code.extend(quote!(,));
            }
            Some(quote! {
                (#code)
            })
        }
        Type::Syn(t) => {
            fn requires_full(features: &Features) -> bool {
                features.any.contains("full") && features.any.len() == 1
            }
            let mut res = simple_visit(t, name);
            let target = defs.types.iter().find(|ty| ty.ident == *t).unwrap();
            if requires_full(&target.features) && !requires_full(features) {
                res = quote!(full!(#res));
            }
            Some(res)
        }
        Type::Ext(t) if r#gen::TERMINAL_TYPES.contains(&&t[..]) => Some(simple_visit(t, name)),
        Type::Ext(_) | Type::Std(_) | Type::Token(_) | Type::Group(_) => None,
    }
}

fn node(traits: &mut TokenStream, impls: &mut TokenStream, s: &Node, defs: &Definitions) {
    let under_name = r#gen::under_name(&s.ident);
    let ty = Ident::new(&s.ident, Span::call_site());
    let fold_fn = format_ident!("fold_{}", under_name);

    let mut fold_impl = TokenStream::new();

    match &s.data {
        Data::Enum(variants) => {
            let mut fold_variants = TokenStream::new();

            for (variant, fields) in variants {
                let variant_ident = Ident::new(variant, Span::call_site());

                if fields.is_empty() {
                    fold_variants.extend(quote! {
                        #ty::#variant_ident => {
                            #ty::#variant_ident
                        }
                    });
                } else {
                    let mut bind_fold_fields = TokenStream::new();
                    let mut fold_fields = TokenStream::new();

                    for (idx, ty) in fields.iter().enumerate() {
                        let binding = format_ident!("_binding_{}", idx);

                        bind_fold_fields.extend(quote! {
                            #binding,
                        });

                        let owned_binding = quote!(#binding);

                        fold_fields.extend(
                            visit(ty, &s.features, defs, &owned_binding).unwrap_or(owned_binding),
                        );

                        fold_fields.extend(quote!(,));
                    }

                    fold_variants.extend(quote! {
                        #ty::#variant_ident(#bind_fold_fields) => {
                            #ty::#variant_ident(
                                #fold_fields
                            )
                        }
                    });
                }
            }

            fold_impl.extend(quote! {
                match node {
                    #fold_variants
                }
            });
        }
        Data::Struct(fields) => {
            let mut fold_fields = TokenStream::new();

            for (field, ty) in fields {
                let id = Ident::new(field, Span::call_site());
                let ref_toks = quote!(node.#id);

                let fold = visit(ty, &s.features, defs, &ref_toks).unwrap_or(ref_toks);

                fold_fields.extend(quote! {
                    #id: #fold,
                });
            }

            if fields.is_empty() {
                if ty == "Ident" {
                    fold_impl.extend(quote! {
                        let mut node = node;
                        let span = f.fold_span(node.span());
                        node.set_span(span);
                    });
                }
                fold_impl.extend(quote! {
                    node
                });
            } else {
                fold_impl.extend(quote! {
                    #ty {
                        #fold_fields
                    }
                });
            }
        }
        Data::Private => {
            if ty == "Ident" {
                fold_impl.extend(quote! {
                    let mut node = node;
                    let span = f.fold_span(node.span());
                    node.set_span(span);
                });
            }
            fold_impl.extend(quote! {
                node
            });
        }
    }

    let fold_span_only =
        s.data == Data::Private && !r#gen::TERMINAL_TYPES.contains(&s.ident.as_str());
    if fold_span_only {
        fold_impl = quote! {
            let span = f.fold_span(node.span());
            let mut node = node;
            node.set_span(span);
            node
        };
    }

    traits.extend(quote! {
        fn #fold_fn(&mut self, i: #ty) -> #ty {
            #fold_fn(self, i)
        }
    });

    impls.extend(quote! {
        pub fn #fold_fn<F>(f: &mut F, node: #ty) -> #ty
        where
            F: Fold + ?Sized,
        {
            #fold_impl
        }
    });
}

pub fn generate(defs: &Definitions) -> Result<()> {
    let (traits, impls) = r#gen::traverse(defs, node);
    let full_macro = full::get_macro();
    file::write(
        FOLD_SRC,
        quote! {
            // Unreachable code is generated sometimes without the full feature.
            #![allow(unreachable_code, unused_variables)]
            #![allow(
                clippy::match_wildcard_for_single_variants,
                clippy::needless_match,
                clippy::needless_pass_by_ref_mut,
            )]

            #[cfg(any(feature = "full", feature = "derive"))]
            use crate::r#gen::helper::fold::*;
            use crate::*;
            use proc_macro2::Span;

            #full_macro

            /// Syntax tree traversal to transform the nodes of an owned syntax tree.
            ///
            /// See the [module documentation] for details.
            ///
            /// [module documentation]: self
            pub trait Fold {
                #traits
            }

            #impls
        },
    )?;
    Ok(())
}
