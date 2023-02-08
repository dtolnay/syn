use crate::{cfg, file, lookup};
use anyhow::Result;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use std::collections::BTreeSet as Set;
use syn_codegen::{Data, Definitions, Node, Type};

const DEBUG_SRC: &str = "src/gen/debug.rs";

fn syntax_tree_enum<'a>(
    enum_name: &str,
    variant_name: &str,
    fields: &'a [Type],
) -> Option<&'a str> {
    if fields.len() != 1 {
        return None;
    }
    const WHITELIST: &[(&str, &str)] = &[
        ("Meta", "Path"),
        ("Pat", "Const"),
        ("Pat", "Lit"),
        ("Pat", "Macro"),
        ("Pat", "Path"),
        ("Pat", "Range"),
        ("PathArguments", "AngleBracketed"),
        ("PathArguments", "Parenthesized"),
        ("Stmt", "Local"),
        ("TypeParamBound", "Lifetime"),
        ("Visibility", "Public"),
        ("Visibility", "Restricted"),
    ];
    match &fields[0] {
        Type::Syn(ty)
            if WHITELIST.contains(&(enum_name, variant_name))
                || enum_name.to_owned() + variant_name == *ty =>
        {
            Some(ty)
        }
        _ => None,
    }
}

fn expand_impl_body(
    defs: &Definitions,
    node: &Node,
    syntax_tree_variants: &Set<&str>,
) -> TokenStream {
    let type_name = &node.ident;
    let ident = Ident::new(type_name, Span::call_site());
    let is_syntax_tree_variant = syntax_tree_variants.contains(type_name.as_str());

    let body = match &node.data {
        Data::Enum(variants) if variants.is_empty() => quote!(match *self {}),
        Data::Enum(variants) => {
            assert!(!is_syntax_tree_variant);
            let arms = variants.iter().map(|(variant_name, fields)| {
                let variant = Ident::new(variant_name, Span::call_site());
                if fields.is_empty() {
                    quote! {
                        #ident::#variant => formatter.write_str(#variant_name),
                    }
                } else {
                    let mut cfg = None;
                    if node.ident == "Expr" {
                        if let Type::Syn(ty) = &fields[0] {
                            if !lookup::node(defs, ty).features.any.contains("derive") {
                                cfg = Some(quote!(#[cfg(feature = "full")]));
                            }
                        }
                    }
                    if syntax_tree_enum(type_name, variant_name, fields).is_some() {
                        quote! {
                            #cfg
                            #ident::#variant(v0) => v0.debug(formatter, #variant_name),
                        }
                    } else {
                        let pats = (0..fields.len())
                            .map(|i| format_ident!("v{}", i))
                            .collect::<Vec<_>>();
                        quote! {
                            #cfg
                            #ident::#variant(#(#pats),*) => {
                                let mut formatter = formatter.debug_tuple(#variant_name);
                                #(formatter.field(#pats);)*
                                formatter.finish()
                            }
                        }
                    }
                }
            });
            let nonexhaustive = if node.ident == "Expr" {
                Some(quote! {
                    #[cfg(not(feature = "full"))]
                    _ => unreachable!(),
                })
            } else {
                None
            };
            let prefix = format!("{}::", type_name);
            quote! {
                formatter.write_str(#prefix)?;
                match self {
                    #(#arms)*
                    #nonexhaustive
                }
            }
        }
        Data::Struct(fields) => {
            let type_name = if is_syntax_tree_variant {
                quote!(name)
            } else {
                quote!(#type_name)
            };
            let fields = fields.keys().map(|f| {
                let ident = Ident::new(f, Span::call_site());
                quote! {
                    formatter.field(#f, &self.#ident);
                }
            });
            quote! {
                let mut formatter = formatter.debug_struct(#type_name);
                #(#fields)*
                formatter.finish()
            }
        }
        Data::Private => unreachable!(),
    };

    if is_syntax_tree_variant {
        quote! {
            impl #ident {
                fn debug(&self, formatter: &mut fmt::Formatter, name: &str) -> fmt::Result {
                    #body
                }
            }
            self.debug(formatter, #type_name)
        }
    } else {
        body
    }
}

fn expand_impl(defs: &Definitions, node: &Node, syntax_tree_variants: &Set<&str>) -> TokenStream {
    let manual_debug = node.data == Data::Private || node.ident == "LitBool";
    if manual_debug {
        return TokenStream::new();
    }

    let ident = Ident::new(&node.ident, Span::call_site());
    let cfg_features = cfg::features(&node.features);
    let body = expand_impl_body(defs, node, syntax_tree_variants);
    let formatter = match &node.data {
        Data::Enum(variants) if variants.is_empty() => quote!(_formatter),
        _ => quote!(formatter),
    };

    quote! {
        #cfg_features
        #[cfg_attr(doc_cfg, doc(cfg(feature = "extra-traits")))]
        impl Debug for #ident {
            fn fmt(&self, #formatter: &mut fmt::Formatter) -> fmt::Result {
                #body
            }
        }
    }
}

pub fn generate(defs: &Definitions) -> Result<()> {
    let mut syntax_tree_variants = Set::new();
    for node in &defs.types {
        if let Data::Enum(variants) = &node.data {
            let enum_name = &node.ident;
            for (variant_name, fields) in variants {
                if let Some(inner) = syntax_tree_enum(enum_name, variant_name, fields) {
                    syntax_tree_variants.insert(inner);
                }
            }
        }
    }

    let mut impls = TokenStream::new();
    for node in &defs.types {
        impls.extend(expand_impl(defs, node, &syntax_tree_variants));
    }

    file::write(
        DEBUG_SRC,
        quote! {
            use crate::*;
            use std::fmt::{self, Debug};

            #impls
        },
    )?;

    Ok(())
}
