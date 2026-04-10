#![recursion_limit = "256"]
#![feature(negative_impls)]
#![allow(
    clippy::elidable_lifetime_names,
    clippy::needless_lifetimes,
    clippy::uninlined_format_args
)]

#[macro_use]
mod snapshot;

mod debug;

use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{Item, ItemTrait};

#[test]
fn test_macro_variable_attr() {
    // mimics the token stream corresponding to `$attr fn f() {}`
    let tokens = TokenStream::from_iter([
        TokenTree::Group(Group::new(Delimiter::None, quote! { #[test] })),
        TokenTree::Ident(Ident::new("fn", Span::call_site())),
        TokenTree::Ident(Ident::new("f", Span::call_site())),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
        TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::new())),
    ]);

    snapshot!(tokens as Item, @r#"
    Item::Fn {
        attrs: [
            Attribute {
                style: AttrStyle::Outer,
                meta: Meta::Path {
                    segments: [
                        PathSegment {
                            ident: "test",
                        },
                    ],
                },
            },
        ],
        vis: Visibility::Inherited,
        sig: Signature {
            ident: "f",
            generics: Generics,
            output: ReturnType::Default,
        },
        block: Block {
            stmts: [],
        },
    }
    "#);
}

#[test]
fn test_negative_impl() {
    #[cfg(any())]
    impl ! {}
    let tokens = quote! {
        impl ! {}
    };
    snapshot!(tokens as Item, @r#"
    Item::Impl {
        generics: Generics,
        self_ty: Type::Never,
    }
    "#);

    let tokens = quote! {
        impl !Trait {}
    };
    let err = syn::parse2::<Item>(tokens).unwrap_err();
    assert_eq!(err.to_string(), "inherent impls cannot be negative");

    #[cfg(any())]
    impl !Trait for T {}
    let tokens = quote! {
        impl !Trait for T {}
    };
    snapshot!(tokens as Item, @r#"
    Item::Impl {
        generics: Generics,
        trait_: Some((
            Some,
            Path {
                segments: [
                    PathSegment {
                        ident: "Trait",
                    },
                ],
            },
        )),
        self_ty: Type::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "T",
                    },
                ],
            },
        },
    }
    "#);
}

#[test]
fn test_macro_variable_impl() {
    // mimics the token stream corresponding to `impl $trait for $ty {}`
    let tokens = TokenStream::from_iter([
        TokenTree::Ident(Ident::new("impl", Span::call_site())),
        TokenTree::Group(Group::new(Delimiter::None, quote!(Trait))),
        TokenTree::Ident(Ident::new("for", Span::call_site())),
        TokenTree::Group(Group::new(Delimiter::None, quote!(Type))),
        TokenTree::Group(Group::new(Delimiter::Brace, TokenStream::new())),
    ]);

    snapshot!(tokens as Item, @r#"
    Item::Impl {
        generics: Generics,
        trait_: Some((
            None,
            Path {
                segments: [
                    PathSegment {
                        ident: "Trait",
                    },
                ],
            },
        )),
        self_ty: Type::Group {
            elem: Type::Path {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "Type",
                        },
                    ],
                },
            },
        },
    }
    "#);
}

#[test]
fn test_supertraits() {
    // Rustc parses all of the following.

    #[rustfmt::skip]
    let tokens = quote!(trait Trait where {});
    snapshot!(tokens as ItemTrait, @r#"
    ItemTrait {
        vis: Visibility::Inherited,
        ident: "Trait",
        generics: Generics {
            where_clause: Some(WhereClause),
        },
    }
    "#);

    #[rustfmt::skip]
    let tokens = quote!(trait Trait: where {});
    snapshot!(tokens as ItemTrait, @r#"
    ItemTrait {
        vis: Visibility::Inherited,
        ident: "Trait",
        generics: Generics {
            where_clause: Some(WhereClause),
        },
        colon_token: Some,
    }
    "#);

    #[rustfmt::skip]
    let tokens = quote!(trait Trait: Sized where {});
    snapshot!(tokens as ItemTrait, @r#"
    ItemTrait {
        vis: Visibility::Inherited,
        ident: "Trait",
        generics: Generics {
            where_clause: Some(WhereClause),
        },
        colon_token: Some,
        supertraits: [
            TypeParamBound::Trait(TraitBound {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "Sized",
                        },
                    ],
                },
            }),
        ],
    }
    "#);

    #[rustfmt::skip]
    let tokens = quote!(trait Trait: Sized + where {});
    snapshot!(tokens as ItemTrait, @r#"
    ItemTrait {
        vis: Visibility::Inherited,
        ident: "Trait",
        generics: Generics {
            where_clause: Some(WhereClause),
        },
        colon_token: Some,
        supertraits: [
            TypeParamBound::Trait(TraitBound {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "Sized",
                        },
                    ],
                },
            }),
            Token![+],
        ],
    }
    "#);
}

#[test]
fn test_type_empty_bounds() {
    #[rustfmt::skip]
    let tokens = quote! {
        trait Foo {
            type Bar: ;
        }
    };

    snapshot!(tokens as ItemTrait, @r#"
    ItemTrait {
        vis: Visibility::Inherited,
        ident: "Foo",
        generics: Generics,
        items: [
            TraitItem::Type {
                ident: "Bar",
                generics: Generics,
                colon_token: Some,
            },
        ],
    }
    "#);
}

#[test]
fn test_impl_visibility() {
    let tokens = quote! {
        pub default unsafe impl union {}
    };

    snapshot!(tokens as Item, @"Item::Verbatim(`pub default unsafe impl union { }`)");
}

#[test]
fn test_impl_type_parameter_defaults() {
    #[cfg(any())]
    impl<T = ()> () {}
    let tokens = quote! {
        impl<T = ()> () {}
    };
    snapshot!(tokens as Item, @r#"
    Item::Impl {
        generics: Generics {
            lt_token: Some,
            params: [
                GenericParam::Type(TypeParam {
                    ident: "T",
                    eq_token: Some,
                    default: Some(Type::Tuple),
                }),
            ],
            gt_token: Some,
        },
        self_ty: Type::Tuple,
    }
    "#);
}

#[test]
fn test_impl_trait_trailing_plus() {
    let tokens = quote! {
        fn f() -> impl Sized + {}
    };

    snapshot!(tokens as Item, @r#"
    Item::Fn {
        vis: Visibility::Inherited,
        sig: Signature {
            ident: "f",
            generics: Generics,
            output: ReturnType::Type(
                Type::ImplTrait {
                    bounds: [
                        TypeParamBound::Trait(TraitBound {
                            path: Path {
                                segments: [
                                    PathSegment {
                                        ident: "Sized",
                                    },
                                ],
                            },
                        }),
                        Token![+],
                    ],
                },
            ),
        },
        block: Block {
            stmts: [],
        },
    }
    "#);
}

// Regression test for issue https://github.com/dtolnay/syn/issues/1967
#[test]
fn test_nested_receiver_classification() {
    let tokens = quote! {
        fn foo(
            self: foo<{ fn foo(
                self: foo<{ fn foo(
                    self: foo<{ fn foo(
                        self: foo<{ fn foo(
                            self: foo<{ fn foo(
                                self: foo<{ fn foo(
                                    self: foo<{ fn foo(
                                        self: foo<{ fn foo(
                                            self: foo<{ fn foo(
                                                self: foo<{ fn foo(
                                                    self: foo<{ fn foo(
                                                        self: foo<{ fn foo(
                                                            self: foo<{ fn foo(
                                                                self: foo<{ fn foo(
                                                                    self: foo<{ fn foo(
                                                                        self: foo<{ fn foo(
                                                                            self: foo<{ fn foo(
                                                                                self: foo<{ fn foo(
                                                                                    self: foo<{ fn foo(
                                                                                        self: foo<{ fn foo(
                                                                                            self: foo<{ fn foo(
                                                                                            )}>
                                                                                        )}>
                                                                                    )}>
                                                                                )}>
                                                                            )}>
                                                                        )}>
                                                                    )}>
                                                                )}>
                                                            )}>
                                                        )}>
                                                    )}>
                                                )}>
                                            )}>
                                        )}>
                                    )}>
                                )}>
                            )}>
                        )}>
                    )}>
                )}>
            )}>
        ) {}
    };

    let _ = syn::parse2::<syn::File>(tokens);
}

// Test for issue https://github.com/dtolnay/syn/issues/1972
#[test]
fn test_const_impl() {
    // Test const impl trait for type
    let tokens = quote! {
        const impl Trait for Type {}
    };
    let parsed: syn::ItemImpl = syn::parse2(tokens).expect("failed to parse const impl");

    // Verify const token is captured
    assert!(parsed.constness.is_some(), "const token should be present in const impl");
    assert!(parsed.trait_.is_some(), "trait_ should be Some for trait impl");
    assert!(parsed.defaultness.is_none(), "default should be None");
    assert!(parsed.unsafety.is_none(), "unsafe should be None");
}

#[test]
fn test_const_impl_round_trip() {
    // Test that const impl round-trips correctly through ToTokens
    let tokens = quote! {
        const impl MyTrait for MyStruct {
            fn foo() -> i32 { 42 }
        }
    };

    let parsed: syn::ItemImpl = syn::parse2(tokens.clone()).expect("failed to parse const impl");
    assert!(parsed.constness.is_some(), "const token should be present");

    // Verify ToTokens produces the same input
    let regenerated = quote!(#parsed);
    assert_eq!(
        tokens.to_string(),
        regenerated.to_string(),
        "const impl should round-trip correctly"
    );
}

#[test]
fn test_const_impl_with_modifiers() {
    // Test const impl combined with other modifiers
    let tokens = quote! {
        default unsafe const impl Trait for Type {}
    };
    snapshot!(tokens as Item, @r#"
    Item::Impl {
        defaultness: Some,
        unsafety: Some,
        constness: Some,
        generics: Generics,
        trait_: Some((
            None,
            Path {
                segments: [
                    PathSegment {
                        ident: "Trait",
                    },
                ],
            },
        )),
        self_ty: Type::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "Type",
                    },
                ],
            },
        },
    }
    "#);
}

#[test]
fn test_regular_impl_without_const() {
    // Verify regular impl still works without const
    let tokens = quote! {
        impl Trait for Type {}
    };
    snapshot!(tokens as Item, @r#"
    Item::Impl {
        generics: Generics,
        trait_: Some((
            None,
            Path {
                segments: [
                    PathSegment {
                        ident: "Trait",
                    },
                ],
            },
        )),
        self_ty: Type::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "Type",
                    },
                ],
            },
        },
    }
    "#);
}

// Test for `impl const Trait for Type {}` syntax (alternative const impl syntax)
#[test]
fn test_impl_const_trait() {
    // Test impl const trait for type (const after impl)
    let tokens = quote! {
        impl const Trait for Type {}
    };
    let parsed: syn::ItemImpl = syn::parse2(tokens).expect("failed to parse impl const");

    // Verify const token is captured
    assert!(parsed.constness.is_some(), "const token should be present in impl const");
    assert!(parsed.trait_.is_some(), "trait_ should be Some for trait impl");
}

#[test]
fn test_impl_const_with_generics() {
    // Test impl const with generic parameters
    let tokens = quote! {
        impl<T> const Trait<T> for Type<T> {}
    };
    let parsed: syn::ItemImpl = syn::parse2(tokens).expect("failed to parse impl const with generics");

    assert!(parsed.constness.is_some(), "const token should be present");
    assert!(parsed.generics.lt_token.is_some(), "generics should be present");
}

#[test]
fn test_impl_const_round_trip() {
    // Test that impl const round-trips correctly through ToTokens
    // Note: ToTokens normalizes to `const impl` syntax regardless of input order
    let tokens = quote! {
        impl const Default for String {
            fn default() -> Self { String::new() }
        }
    };

    let parsed: syn::ItemImpl = syn::parse2(tokens).expect("failed to parse impl const");
    assert!(parsed.constness.is_some(), "const token should be present");

    // Verify ToTokens produces valid output (normalized to `const impl`)
    let regenerated = quote!(#parsed);
    let regenerated_str = regenerated.to_string();
    assert!(
        regenerated_str.contains("const impl"),
        "ToTokens should produce `const impl` syntax, got: {}",
        regenerated_str
    );

    // Verify the regenerated code can be parsed again
    let reparsed: syn::ItemImpl = syn::parse2(regenerated).expect("failed to re-parse");
    assert!(reparsed.constness.is_some(), "const token should be preserved after round-trip");
}
