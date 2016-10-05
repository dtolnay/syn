extern crate syn;
use syn::*;

fn simple_path(name: &'static str) -> Path {
    Path {
        global: false,
        segments: vec![
            PathSegment {
                ident: Ident::new(name),
                parameters: PathParameters::none(),
            },
        ],
    }
}

#[test]
fn test_split_for_impl() {
    // <'a, 'b: 'a, T: 'a = ()> where T: Debug
    let generics = Generics {
        lifetimes: vec![
            LifetimeDef {
                lifetime: Lifetime::new("'a"),
                bounds: Vec::new(),
            },
            LifetimeDef {
                lifetime: Lifetime::new("'b"),
                bounds: vec![
                    Lifetime::new("'a"),
                ],
            },
        ],
        ty_params: vec![
            TyParam {
                ident: Ident::new("T"),
                bounds: vec![
                    TyParamBound::Region(Lifetime::new("'a")),
                ],
                default: Some(Ty::Tup(Vec::new())),
            },
        ],
        where_clause: WhereClause {
            predicates: vec![
                WherePredicate::BoundPredicate(WhereBoundPredicate {
                    bound_lifetimes: Vec::new(),
                    bounded_ty: Ty::Path(None, simple_path("T")),
                    bounds: vec![
                        TyParamBound::Trait(
                            PolyTraitRef {
                                bound_lifetimes: Vec::new(),
                                trait_ref: simple_path("Debug"),
                            },
                            TraitBoundModifier::None,
                        ),
                    ],
                }),
            ],
        },
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // <'a, 'b: 'a, T: 'a>
    let expected_impl_generics = Generics {
        lifetimes: vec![
            LifetimeDef {
                lifetime: Lifetime::new("'a"),
                bounds: Vec::new(),
            },
            LifetimeDef {
                lifetime: Lifetime::new("'b"),
                bounds: vec![
                    Lifetime::new("'a"),
                ],
            },
        ],
        ty_params: vec![
            TyParam {
                ident: Ident::new("T"),
                bounds: vec![
                    TyParamBound::Region(Lifetime::new("'a")),
                ],
                default: None,
            },
        ],
        where_clause: WhereClause::none(),
    };

    // <'a, 'b, T>
    let expected_ty_generics = Generics {
        lifetimes: vec![
            LifetimeDef {
                lifetime: Lifetime::new("'a"),
                bounds: Vec::new(),
            },
            LifetimeDef {
                lifetime: Lifetime::new("'b"),
                bounds: Vec::new(),
            },
        ],
        ty_params: vec![
            TyParam {
                ident: Ident::new("T"),
                bounds: Vec::new(),
                default: None,
            },
        ],
        where_clause: WhereClause::none(),
    };

    assert_eq!(impl_generics, expected_impl_generics);
    assert_eq!(ty_generics, expected_ty_generics);
    assert_eq!(where_clause, generics.where_clause);
}
