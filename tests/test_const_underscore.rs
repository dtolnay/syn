#[macro_use]
mod macros;

use quote::quote;
use syn::{ItemImpl, ItemTrait};

#[test]
fn test_const_underscore_in_trait() {
    let input = quote! {
        pub trait A {
            const _: () = ();
        }
    };

    snapshot!(input as ItemTrait, @r###"
    ItemTrait {
        vis: Visibility::Public,
        ident: "A",
        generics: Generics,
        items: [
            TraitItem::Const {
                ident: "_",
                ty: Type::Tuple,
                default: Some(Expr::Tuple),
            },
        ],
    }
    "###);
}

#[test]
fn test_const_underscore_in_trait_impl() {
    let input = quote! {
        impl A for () {
            const _: () = ();
        }
    };

    snapshot!(input as ItemImpl, @r###"
    ItemImpl {
        generics: Generics,
        trait_: Some((
            None,
            Path {
                segments: [
                    PathSegment {
                        ident: "A",
                        arguments: None,
                    },
                ],
            },
        )),
        self_ty: Type::Tuple,
        items: [
            ImplItem::Const {
                vis: Inherited,
                ident: "_",
                ty: Type::Tuple,
                expr: Expr::Tuple,
            },
        ],
    }
    "###);
}

#[test]
fn test_const_underscore_in_inherent_impl() {
    let input = quote! {
        impl dyn A {
            const _: () = ();
        }
    };

    snapshot!(input as ItemImpl, @r###"
    ItemImpl {
        generics: Generics,
        self_ty: Type::TraitObject {
            dyn_token: Some,
            bounds: [
                Trait(TraitBound {
                    modifier: None,
                    path: Path {
                        segments: [
                            PathSegment {
                                ident: "A",
                                arguments: None,
                            },
                        ],
                    },
                }),
            ],
        },
        items: [
            ImplItem::Const {
                vis: Inherited,
                ident: "_",
                ty: Type::Tuple,
                expr: Expr::Tuple,
            },
        ],
    }
    "###);
}
