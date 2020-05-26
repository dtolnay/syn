use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use std::iter::FromIterator;
use syn::{Expr, Type};

#[test]
fn parse_interpolated_leading_component() {
    // mimics the token stream corresponding to `$mod::rest`
    let tokens = TokenStream::from_iter(vec![
        TokenTree::Group(Group::new(
            Delimiter::None,
            TokenStream::from_iter(vec![TokenTree::Ident(Ident::new(
                "first",
                Span::call_site(),
            ))]),
        )),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("rest", Span::call_site())),
    ]);

    match syn::parse2::<Expr>(tokens.clone()) {
        Ok(Expr::Path(_)) => {}
        expr => panic!("incorrect expr: {:?}", expr),
    }

    match syn::parse2::<Type>(tokens) {
        Ok(Type::Path(_)) => {}
        ty => panic!("incorrect ty: {:?}", ty),
    }
}
