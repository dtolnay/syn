// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]
#![recursion_limit = "1024"]
#![feature(rustc_private)]

extern crate syn;
use syn::*;

#[macro_use]
extern crate quote;

extern crate proc_macro2;
use proc_macro2::{Ident, Span, TokenStream};

#[macro_use]
mod macros;

mod common;

fn ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}

#[test]
fn test_split_for_impl() {
    // <'a, 'b: 'a, #[may_dangle] T: 'a = ()> where T: Debug
    let generics = Generics {
        gt_token: Some(Default::default()),
        lt_token: Some(Default::default()),
        params: punctuated![
            GenericParam::Lifetime(LifetimeDef {
                attrs: Default::default(),
                lifetime: Lifetime::new("'a", Span::call_site()),
                bounds: Default::default(),
                colon_token: None,
            }),
            GenericParam::Lifetime(LifetimeDef {
                attrs: Default::default(),
                lifetime: Lifetime::new("'b", Span::call_site()),
                bounds: punctuated![Lifetime::new("'a", Span::call_site())],
                colon_token: Some(token::Colon::default()),
            }),
            GenericParam::Type(TypeParam {
                attrs: vec![Attribute {
                    bracket_token: Default::default(),
                    pound_token: Default::default(),
                    style: AttrStyle::Outer,
                    path: ident("may_dangle").into(),
                    tts: TokenStream::new(),
                }],
                ident: ident("T"),
                bounds: punctuated![TypeParamBound::Lifetime(Lifetime::new(
                    "'a",
                    Span::call_site()
                )),],
                default: Some(
                    TypeTuple {
                        elems: Default::default(),
                        paren_token: Default::default(),
                    }
                    .into(),
                ),
                colon_token: Some(Default::default()),
                eq_token: Default::default(),
            }),
        ],
        where_clause: Some(WhereClause {
            where_token: Default::default(),
            predicates: punctuated![WherePredicate::Type(PredicateType {
                lifetimes: None,
                colon_token: Default::default(),
                bounded_ty: TypePath {
                    qself: None,
                    path: ident("T").into(),
                }
                .into(),
                bounds: punctuated![TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: ident("Debug").into(),
                }),],
            }),],
        }),
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let tokens = quote! {
        impl #impl_generics MyTrait for Test #ty_generics #where_clause {}
    };
    let expected = concat!(
        "impl < 'a , 'b : 'a , # [ may_dangle ] T : 'a > ",
        "MyTrait for Test < 'a , 'b , T > ",
        "where T : Debug { }"
    );
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
    let expected = TypeParamBound::Lifetime(Lifetime::new("'a", Span::call_site()));
    assert_eq!(expected, syn::parse2::<TypeParamBound>(tokens).unwrap());

    let tokens = quote!('_);
    println!("{:?}", tokens);
    let expected = TypeParamBound::Lifetime(Lifetime::new("'_", Span::call_site()));
    assert_eq!(expected, syn::parse2::<TypeParamBound>(tokens).unwrap());

    let tokens = quote!(Debug);
    let expected = TypeParamBound::Trait(TraitBound {
        paren_token: None,
        modifier: TraitBoundModifier::None,
        lifetimes: None,
        path: ident("Debug").into(),
    });
    assert_eq!(expected, syn::parse2::<TypeParamBound>(tokens).unwrap());

    let tokens = quote!(?Sized);
    let expected = TypeParamBound::Trait(TraitBound {
        paren_token: None,
        modifier: TraitBoundModifier::Maybe(Default::default()),
        lifetimes: None,
        path: ident("Sized").into(),
    });
    assert_eq!(expected, syn::parse2::<TypeParamBound>(tokens).unwrap());
}

#[test]
fn test_fn_precedence_in_where_clause() {
    // This should parse as two separate bounds, `FnOnce() -> i32` and `Send` - not `FnOnce() -> (i32 + Send)`.
    let sig = quote! {
        fn f<G>()
        where
            G: FnOnce() -> i32 + Send,
        {
        }
    };
    let fun = syn::parse2::<ItemFn>(sig).unwrap();
    let where_clause = fun.decl.generics.where_clause.as_ref().unwrap();
    assert_eq!(where_clause.predicates.len(), 1);
    let predicate = match where_clause.predicates[0] {
        WherePredicate::Type(ref pred) => pred,
        _ => panic!("wrong predicate kind"),
    };
    assert_eq!(predicate.bounds.len(), 2, "{:#?}", predicate.bounds);
    let first_bound = &predicate.bounds[0];
    assert_eq!(quote!(#first_bound).to_string(), "FnOnce ( ) -> i32");
    let second_bound = &predicate.bounds[1];
    assert_eq!(quote!(#second_bound).to_string(), "Send");
}

#[test]
fn test_where_clause_at_end_of_input() {
    let tokens = quote! {
        where
    };
    let where_clause = syn::parse2::<WhereClause>(tokens).unwrap();
    assert_eq!(where_clause.predicates.len(), 0);
}
