use crate::error::Result;
use crate::operand::{Borrowed, Operand, Owned};
use crate::{file, full, gen};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::Index;
use syn_codegen::{Data, Definitions, Features, Node, Type};

const VISIT_SRC: &str = "../src/gen/visit.rs";

fn simple_visit(item: &str, name: &Operand) -> TokenStream {
    let ident = gen::under_name(item);
    let method = Ident::new(&format!("visit_{}", ident), Span::call_site());
    let name = name.ref_tokens();
    quote! {
        _visitor.#method(#name)
    }
}

fn noop_visit(name: &Operand) -> TokenStream {
    let name = name.tokens();
    quote! {
        skip!(#name)
    }
}

fn visit(
    ty: &Type,
    features: &Features,
    defs: &Definitions,
    name: &Operand,
) -> Option<TokenStream> {
    match ty {
        Type::Box(t) => {
            let name = name.owned_tokens();
            visit(t, features, defs, &Owned(quote!(*#name)))
        }
        Type::Vec(t) => {
            let operand = Borrowed(quote!(it));
            let val = visit(t, features, defs, &operand)?;
            let name = name.ref_tokens();
            Some(quote! {
                for it in #name {
                    #val
                }
            })
        }
        Type::Punctuated(p) => {
            let operand = Borrowed(quote!(it));
            let val = visit(&p.element, features, defs, &operand)?;
            let name = name.ref_tokens();
            Some(quote! {
                for el in Punctuated::pairs(#name) {
                    let it = el.value();
                    #val
                }
            })
        }
        Type::Option(t) => {
            let it = Borrowed(quote!(it));
            let val = visit(t, features, defs, &it)?;
            let name = name.owned_tokens();
            Some(quote! {
                if let Some(ref it) = #name {
                    #val
                }
            })
        }
        Type::Tuple(t) => {
            let mut code = TokenStream::new();
            for (i, elem) in t.iter().enumerate() {
                let name = name.tokens();
                let i = Index::from(i);
                let it = Owned(quote!((#name).#i));
                let val = visit(elem, features, defs, &it).unwrap_or_else(|| noop_visit(&it));
                code.extend(val);
                code.extend(quote!(;));
            }
            Some(code)
        }
        Type::Token(t) => {
            let name = name.tokens();
            let repr = &defs.tokens[t];
            let is_keyword = repr.chars().next().unwrap().is_alphabetic();
            let spans = if is_keyword {
                quote!(span)
            } else {
                quote!(spans)
            };
            Some(quote! {
                tokens_helper(_visitor, &#name.#spans)
            })
        }
        Type::Group(_) => {
            let name = name.tokens();
            Some(quote! {
                tokens_helper(_visitor, &#name.span)
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
        Type::Ext(t) if gen::TERMINAL_TYPES.contains(&&t[..]) => Some(simple_visit(t, name)),
        Type::Ext(_) | Type::Std(_) => None,
    }
}

fn node(traits: &mut TokenStream, impls: &mut TokenStream, s: &Node, defs: &Definitions) {
    let under_name = gen::under_name(&s.ident);
    let ty = Ident::new(&s.ident, Span::call_site());
    let visit_fn = Ident::new(&format!("visit_{}", under_name), Span::call_site());

    let mut visit_impl = TokenStream::new();

    match &s.data {
        Data::Enum(variants) => {
            let mut visit_variants = TokenStream::new();

            for (variant, fields) in variants {
                let variant_ident = Ident::new(variant, Span::call_site());

                if fields.is_empty() {
                    visit_variants.extend(quote! {
                        #ty::#variant_ident => {}
                    });
                } else {
                    let mut bind_visit_fields = TokenStream::new();
                    let mut visit_fields = TokenStream::new();

                    for (idx, ty) in fields.iter().enumerate() {
                        let name = format!("_binding_{}", idx);
                        let binding = Ident::new(&name, Span::call_site());

                        bind_visit_fields.extend(quote! {
                            ref #binding,
                        });

                        let borrowed_binding = Borrowed(quote!(#binding));

                        visit_fields.extend(
                            visit(ty, &s.features, defs, &borrowed_binding)
                                .unwrap_or_else(|| noop_visit(&borrowed_binding)),
                        );

                        visit_fields.extend(quote!(;));
                    }

                    visit_variants.extend(quote! {
                        #ty::#variant_ident(#bind_visit_fields) => {
                            #visit_fields
                        }
                    });
                }
            }

            visit_impl.extend(quote! {
                match *_i {
                    #visit_variants
                }
            });
        }
        Data::Struct(fields) => {
            for (field, ty) in fields {
                let id = Ident::new(&field, Span::call_site());
                let ref_toks = Owned(quote!(_i.#id));
                let visit_field = visit(&ty, &s.features, defs, &ref_toks)
                    .unwrap_or_else(|| noop_visit(&ref_toks));
                visit_impl.extend(quote! {
                    #visit_field;
                });
            }
        }
        Data::Private => {}
    }

    traits.extend(quote! {
        fn #visit_fn(&mut self, i: &'ast #ty) {
            #visit_fn(self, i)
        }
    });

    impls.extend(quote! {
        pub fn #visit_fn<'ast, V: Visit<'ast> + ?Sized>(
            _visitor: &mut V, _i: &'ast #ty
        ) {
            #visit_impl
        }
    });
}

pub fn generate(defs: &Definitions) -> Result<()> {
    let (traits, impls) = gen::traverse(defs, node);
    let full_macro = full::get_macro();
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

            #[cfg(any(feature = "full", feature = "derive"))]
            macro_rules! skip {
                ($($tt:tt)*) => {};
            }

            /// Syntax tree traversal to walk a shared borrow of a syntax tree.
            ///
            /// See the [module documentation] for details.
            ///
            /// [module documentation]: index.html
            ///
            /// *This trait is available if Syn is built with the `"visit"` feature.*
            pub trait Visit<'ast> {
                #traits
            }

            #impls
        },
    )?;
    Ok(())
}
