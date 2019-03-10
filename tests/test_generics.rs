extern crate quote;
extern crate syn;

mod features;

#[macro_use]
mod macros;

use quote::quote;
use syn::{DeriveInput, ItemFn, TypeParamBound, WhereClause, WherePredicate};

#[test]
fn test_split_for_impl() {
    let code = quote! {
        struct S<'a, 'b: 'a, #[may_dangle] T: 'a = ()> where T: Debug;
    };

    let actual = snapshot!(code as DeriveInput);

    let generics = actual.generics;
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
    snapshot!(tokens as TypeParamBound);

    let tokens = quote!('_);
    snapshot!(tokens as TypeParamBound);

    let tokens = quote!(Debug);
    snapshot!(tokens as TypeParamBound);

    let tokens = quote!(?Sized);
    snapshot!(tokens as TypeParamBound);
}

#[test]
fn test_fn_precedence_in_where_clause() {
    // This should parse as two separate bounds, `FnOnce() -> i32` and `Send` - not
    // `FnOnce() -> (i32 + Send)`.
    let code = quote! {
        fn f<G>()
        where
            G: FnOnce() -> i32 + Send,
        {
        }
    };

    let actual = snapshot!(code as ItemFn);

    let where_clause = actual.decl.generics.where_clause.as_ref().unwrap();
    assert_eq!(where_clause.predicates.len(), 1);

    let predicate = match &where_clause.predicates[0] {
        WherePredicate::Type(pred) => pred,
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

    let where_clause = snapshot!(tokens as WhereClause);
    assert_eq!(where_clause.predicates.len(), 0);
}
