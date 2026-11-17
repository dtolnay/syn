use proc_macro2::Span;
use quote::quote;
use syn::{
    custom_keyword,
    parse::{Parse, ParseStream, Parser},
};

custom_keyword!(implicit);
custom_keyword!(explicit as Explicit);

#[test]
fn parsing() {
    (|input: ParseStream| {
        assert!(input.peek(implicit));
        implicit::parse(input).unwrap();
        assert!(input.peek(Explicit));
        Explicit::parse(input).unwrap();
        Ok(())
    })
    .parse2(quote!(implicit explicit))
    .unwrap();
}

#[test]
fn printing() {
    let implicit = implicit(Span::call_site());
    let explicit = Explicit(Span::call_site());

    assert_eq!(
        format!("{implicit:?} {explicit:?}"),
        "Keyword [implicit] Keyword [explicit]"
    );
    assert_eq!(quote!(#implicit #explicit).to_string(), "implicit explicit");
}
