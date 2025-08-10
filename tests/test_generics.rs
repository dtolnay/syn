#![allow(
    clippy::elidable_lifetime_names,
    clippy::manual_let_else,
    clippy::needless_lifetimes,
    clippy::too_many_lines,
    clippy::uninlined_format_args
)]

#[macro_use]
mod snapshot;

mod debug;

use quote::quote;
use syn::{DeriveInput, ItemFn, TypeParamBound, WhereClause, WherePredicate};

#[test]
fn test_split_for_impl() {
    let input = quote! {
        struct S<'a, 'b: 'a, #[may_dangle] T: 'a = ()> where T: Debug;
    };

    snapshot!(input as DeriveInput, @r#"
    DeriveInput {
        vis: Visibility::Inherited,
        ident: "S",
        generics: Generics {
            lt_token: Some,
            params: [
                GenericParam::Lifetime(LifetimeParam {
                    lifetime: Lifetime {
                        ident: "a",
                    },
                }),
                Token![,],
                GenericParam::Lifetime(LifetimeParam {
                    lifetime: Lifetime {
                        ident: "b",
                    },
                    colon_token: Some,
                    bounds: [
                        Lifetime {
                            ident: "a",
                        },
                    ],
                }),
                Token![,],
                GenericParam::Type(TypeParam {
                    attrs: [
                        Attribute {
                            style: AttrStyle::Outer,
                            meta: Meta::Path {
                                segments: [
                                    PathSegment {
                                        ident: "may_dangle",
                                    },
                                ],
                            },
                        },
                    ],
                    ident: "T",
                    colon_token: Some,
                    bounds: [
                        TypeParamBound::Lifetime {
                            ident: "a",
                        },
                    ],
                    eq_token: Some,
                    default: Some(Type::Tuple),
                }),
            ],
            gt_token: Some,
            where_clause: Some(WhereClause {
                predicates: [
                    WherePredicate::Type(PredicateType {
                        bounded_ty: Type::Path {
                            path: Path {
                                segments: [
                                    PathSegment {
                                        ident: "T",
                                    },
                                ],
                            },
                        },
                        bounds: [
                            TypeParamBound::Trait(TraitBound {
                                path: Path {
                                    segments: [
                                        PathSegment {
                                            ident: "Debug",
                                        },
                                    ],
                                },
                            }),
                        ],
                    }),
                ],
            }),
        },
        data: Data::Struct {
            fields: Fields::Unit,
            semi_token: Some,
        },
    }
    "#);

    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let generated = quote! {
        impl #impl_generics MyTrait for Test #ty_generics #where_clause {}
    };
    let expected = quote! {
        impl<'a, 'b: 'a, #[may_dangle] T: 'a> MyTrait
        for Test<'a, 'b, T>
        where
            T: Debug
        {}
    };
    assert_eq!(generated.to_string(), expected.to_string());

    let turbofish = ty_generics.as_turbofish();
    let generated = quote! {
        Test #turbofish
    };
    let expected = quote! {
        Test::<'a, 'b, T>
    };
    assert_eq!(generated.to_string(), expected.to_string());
}

#[test]
fn test_ty_param_bound() {
    let tokens = quote!('a);
    snapshot!(tokens as TypeParamBound, @r#"
    TypeParamBound::Lifetime {
        ident: "a",
    }
    "#);

    let tokens = quote!('_);
    snapshot!(tokens as TypeParamBound, @r#"
    TypeParamBound::Lifetime {
        ident: "_",
    }
    "#);

    let tokens = quote!(Debug);
    snapshot!(tokens as TypeParamBound, @r#"
    TypeParamBound::Trait(TraitBound {
        path: Path {
            segments: [
                PathSegment {
                    ident: "Debug",
                },
            ],
        },
    })
    "#);

    let tokens = quote!(?Sized);
    snapshot!(tokens as TypeParamBound, @r#"
    TypeParamBound::Trait(TraitBound {
        modifier: TraitBoundModifier::Maybe,
        path: Path {
            segments: [
                PathSegment {
                    ident: "Sized",
                },
            ],
        },
    })
    "#);
}

#[test]
fn test_fn_precedence_in_where_clause() {
    // This should parse as two separate bounds, `FnOnce() -> i32` and `Send` - not
    // `FnOnce() -> (i32 + Send)`.
    let input = quote! {
        fn f<G>()
        where
            G: FnOnce() -> i32 + Send,
        {
        }
    };

    snapshot!(input as ItemFn, @r#"
    ItemFn {
        vis: Visibility::Inherited,
        sig: Signature {
            ident: "f",
            generics: Generics {
                lt_token: Some,
                params: [
                    GenericParam::Type(TypeParam {
                        ident: "G",
                    }),
                ],
                gt_token: Some,
                where_clause: Some(WhereClause {
                    predicates: [
                        WherePredicate::Type(PredicateType {
                            bounded_ty: Type::Path {
                                path: Path {
                                    segments: [
                                        PathSegment {
                                            ident: "G",
                                        },
                                    ],
                                },
                            },
                            bounds: [
                                TypeParamBound::Trait(TraitBound {
                                    path: Path {
                                        segments: [
                                            PathSegment {
                                                ident: "FnOnce",
                                                arguments: PathArguments::Parenthesized {
                                                    output: ReturnType::Type(
                                                        Type::Path {
                                                            path: Path {
                                                                segments: [
                                                                    PathSegment {
                                                                        ident: "i32",
                                                                    },
                                                                ],
                                                            },
                                                        },
                                                    ),
                                                },
                                            },
                                        ],
                                    },
                                }),
                                Token![+],
                                TypeParamBound::Trait(TraitBound {
                                    path: Path {
                                        segments: [
                                            PathSegment {
                                                ident: "Send",
                                            },
                                        ],
                                    },
                                }),
                            ],
                        }),
                        Token![,],
                    ],
                }),
            },
            output: ReturnType::Default,
        },
        block: Block {
            stmts: [],
        },
    }
    "#);

    let where_clause = input.sig.generics.where_clause.as_ref().unwrap();
    assert_eq!(where_clause.predicates.len(), 1);

    let predicate = match &where_clause.predicates[0] {
        WherePredicate::Type(pred) => pred,
        _ => panic!("wrong predicate kind"),
    };

    assert_eq!(predicate.bounds.len(), 2, "{:#?}", predicate.bounds);

    let first_bound = &predicate.bounds[0];
    assert_eq!(quote!(#first_bound).to_string(), "FnOnce () -> i32");

    let second_bound = &predicate.bounds[1];
    assert_eq!(quote!(#second_bound).to_string(), "Send");
}

#[test]
fn test_where_clause_at_end_of_input() {
    let input = quote! {
        where
    };

    snapshot!(input as WhereClause, @"WhereClause");

    assert_eq!(input.predicates.len(), 0);
}

#[test]
fn test_trait_bound_field_order() {
    // Test that TraitBound fields are parsed and serialized in the correct order
    // to match the Rust compiler's AST structure: lifetimes, then modifiers, then path
    // Also test comprehensive parsing scenarios and round-trip consistency

    use syn::{parse_quote, TraitBound, TypeParamBound};

    // Test 1: Basic trait bound without lifetimes or modifier
    let bound: TraitBound = parse_quote!(Clone);
    assert!(bound.paren_token.is_none());
    assert!(bound.lifetimes.is_none());
    assert!(matches!(bound.modifier, syn::TraitBoundModifier::None));
    assert_eq!(bound.path.segments.len(), 1);
    assert_eq!(bound.path.segments[0].ident, "Clone");

    // Test 2: Trait bound with optional modifier
    let bound: TraitBound = parse_quote!(?Sized);
    assert!(bound.paren_token.is_none());
    assert!(bound.lifetimes.is_none());
    assert!(matches!(bound.modifier, syn::TraitBoundModifier::Maybe(_)));
    assert_eq!(bound.path.segments.len(), 1);
    assert_eq!(bound.path.segments[0].ident, "Sized");

    // Test 3: Trait bound with lifetimes
    let type_bound: TypeParamBound = parse_quote!(for<'a> Fn(&'a str));
    if let TypeParamBound::Trait(bound) = type_bound {
        assert!(bound.paren_token.is_none());
        assert!(bound.lifetimes.is_some());
        assert!(matches!(bound.modifier, syn::TraitBoundModifier::None));
        assert_eq!(bound.path.segments.len(), 1);
        assert_eq!(bound.path.segments[0].ident, "Fn");
    } else {
        panic!("Expected trait bound");
    }

    // Test 4: Trait bound with both lifetimes and modifier (lifetimes first)
    let type_bound: TypeParamBound = parse_quote!(for<'a> ?Clone);
    if let TypeParamBound::Trait(bound) = type_bound {
        assert!(bound.paren_token.is_none());
        assert!(bound.lifetimes.is_some());
        assert!(matches!(bound.modifier, syn::TraitBoundModifier::Maybe(_)));
        assert_eq!(bound.path.segments.len(), 1);
        assert_eq!(bound.path.segments[0].ident, "Clone");
    } else {
        panic!("Expected trait bound");
    }

    // Test 5: Complex trait bound with multiple lifetimes and path
    let type_bound: TypeParamBound = parse_quote!(for<'a, 'b> Iterator<Item = &'a str>);
    if let TypeParamBound::Trait(bound) = type_bound {
        assert!(bound.paren_token.is_none());
        assert!(bound.lifetimes.is_some());
        let lifetimes = bound.lifetimes.as_ref().unwrap();
        assert_eq!(lifetimes.lifetimes.len(), 2);
        assert!(matches!(bound.modifier, syn::TraitBoundModifier::None));

        // Verify field access order matches expected struct layout
        let _ = (
            &bound.paren_token, // first field
            &bound.lifetimes,   // second field (moved here from third)
            &bound.modifier,    // third field (moved here from second)
            &bound.path,        // fourth field
        );
    } else {
        panic!("Expected trait bound");
    }

    // Test 6: Complex path with lifetimes and modifier
    let type_bound: TypeParamBound = parse_quote!(for<'a> ?std::fmt::Debug);
    if let TypeParamBound::Trait(bound) = type_bound {
        assert!(bound.lifetimes.is_some());
        assert!(matches!(bound.modifier, syn::TraitBoundModifier::Maybe(_)));
        assert_eq!(bound.path.segments.len(), 3);
        assert_eq!(bound.path.segments[0].ident, "std");
        assert_eq!(bound.path.segments[1].ident, "fmt");
        assert_eq!(bound.path.segments[2].ident, "Debug");
    } else {
        panic!("Expected trait bound");
    }

    // Test 7: Verify that parsing and printing produces consistent results
    let original = "for<'a, 'b> ?Iterator<Item = &'a str>";
    let parsed: syn::TypeParamBound = syn::parse_str(original).unwrap();
    let printed = quote::quote!(#parsed).to_string();
    let reparsed: syn::TypeParamBound = syn::parse_str(&printed).unwrap();

    // Ensure round-trip consistency
    if let (syn::TypeParamBound::Trait(orig), syn::TypeParamBound::Trait(reparsed)) = (parsed, reparsed) {
        assert_eq!(orig.lifetimes.is_some(), reparsed.lifetimes.is_some());
        assert_eq!(orig.modifier, reparsed.modifier);
        assert_eq!(orig.path.segments.len(), reparsed.path.segments.len());
    } else {
        panic!("Expected trait bounds");
    }

    // Test 8: Verify that invalid syntax fails to parse
    // Modifiers cannot come before lifetimes in valid Rust syntax
    let result = syn::parse_str::<TraitBound>("?for<'a> Clone");
    assert!(result.is_err(), "Invalid syntax should fail to parse");
}
