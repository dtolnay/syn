#![cfg(feature = "extra-traits")]

extern crate syn;
use syn::*;

#[macro_use]
extern crate quote;

extern crate proc_macro2;
use proc_macro2::Term;

mod common;

#[test]
fn test_split_for_impl() {
    // <'a, 'b: 'a, #[may_dangle] T: 'a = ()> where T: Debug
    let mut generics = Generics {
        gt_token: Some(Default::default()),
        lt_token: Some(Default::default()),
        lifetimes: vec![
            LifetimeDef {
                attrs: Default::default(),
                lifetime: Lifetime::new(Term::intern("'a"), Span::default()),
                bounds: Default::default(),
                colon_token: None,
            },
            LifetimeDef {
                attrs: Default::default(),
                lifetime: Lifetime::new(Term::intern("'b"), Span::default()),
                bounds: vec![Lifetime::new(Term::intern("'a"), Span::default())].into(),
                colon_token: Some(tokens::Colon::default()),
            },
        ].into(),
        ty_params: vec![
            TyParam {
                attrs: vec![Attribute {
                    bracket_token: Default::default(),
                    pound_token: Default::default(),
                    style: AttrStyle::Outer,
                    path: "may_dangle".into(),
                    tts: vec![],
                    is_sugared_doc: false,
                }],
                ident: "T".into(),
                bounds: vec![TyParamBound::Region(Lifetime::new(Term::intern("'a"), Span::default()))].into(),
                default: Some(TyTup {
                    tys: Default::default(),
                    lone_comma: None,
                    paren_token: Default::default(),
                }.into()),
                colon_token: Some(Default::default()),
                eq_token: Default::default(),
            },
        ].into(),
        where_clause: WhereClause {
            where_token: Some(Default::default()),
            predicates: vec![
                WherePredicate::BoundPredicate(WhereBoundPredicate {
                    bound_lifetimes: None,
                    colon_token: Default::default(),
                    bounded_ty: TyPath {
                        qself: None,
                        path: "T".into(),
                    }.into(),
                    bounds: vec![
                        TyParamBound::Trait(
                            PolyTraitRef {
                                bound_lifetimes: None,
                                trait_ref: "Debug".into(),
                            },
                            TraitBoundModifier::None,
                        ),
                    ].into(),
                })
            ].into(),
        },
    };
    generics.lifetimes.push_trailing(Default::default());

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let tokens = quote! {
        impl #impl_generics MyTrait for Test #ty_generics #where_clause {}
    };
    let expected = concat!("impl < 'a , 'b : 'a , # [ may_dangle ] T : 'a > ",
                           "MyTrait for Test < 'a , 'b , T > ",
                           "where T : Debug { }");
    assert_eq!(expected, tokens.to_string());

    let turbofish = ty_generics.as_turbofish();
    let tokens = quote! {
        Test #turbofish
    };
    let expected = "Test :: < 'a , 'b , T >";
    assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_ty_param_bound() {
    let tokens = quote!('a);
    let expected = TyParamBound::Region(Lifetime::new(Term::intern("'a"), Span::default()));
    assert_eq!(expected, common::parse::syn::<TyParamBound>(tokens.into()));

    let tokens = quote!(Debug);
    let expected = TyParamBound::Trait(
        PolyTraitRef {
            bound_lifetimes: None,
            trait_ref: "Debug".into(),
        },
        TraitBoundModifier::None);
    assert_eq!(expected, common::parse::syn::<TyParamBound>(tokens.into()));

    let tokens = quote!(?Sized);
    let expected = TyParamBound::Trait(
        PolyTraitRef {
            bound_lifetimes: None,
            trait_ref: "Sized".into(),
        },
        TraitBoundModifier::Maybe(Default::default()));
    assert_eq!(expected, common::parse::syn::<TyParamBound>(tokens.into()));
}
