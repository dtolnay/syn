#[macro_use]
mod macros;

use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::quote;
use std::iter::FromIterator;
use syn::{Expr, Type};

#[test]
fn parse_interpolated_leading_component() {
    // mimics the token stream corresponding to `$mod::rest`
    let tokens = TokenStream::from_iter(vec![
        TokenTree::Group(Group::new(Delimiter::None, quote! { first })),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("rest", Span::call_site())),
    ]);

    snapshot!(tokens.clone() as Expr, @r###"
    Expr::Path {
        path: Path {
            segments: [
                PathSegment {
                    ident: "first",
                    arguments: None,
                },
                PathSegment {
                    ident: "rest",
                    arguments: None,
                },
            ],
        },
    }
    "###);

    snapshot!(tokens as Type, @r###"
    Type::Path {
        path: Path {
            segments: [
                PathSegment {
                    ident: "first",
                    arguments: None,
                },
                PathSegment {
                    ident: "rest",
                    arguments: None,
                },
            ],
        },
    }
    "###);
}

#[test]
fn print_incomplete_qpath() {
    use quote::ToTokens;
    use syn::TypePath;

    // qpath with `as` token
    let mut ty: TypePath = syn::parse_quote!(<Self as A>::Q);
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self as A > :: Q`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self as A > ::`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self >`)
    "###);
    assert!(ty.path.segments.pop().is_none());

    // qpath without `as` token
    let mut ty: TypePath = syn::parse_quote!(<Self>::A::B);
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self > :: A :: B`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self > :: A ::`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`< Self > ::`)
    "###);
    assert!(ty.path.segments.pop().is_none());

    // normal path
    let mut ty: TypePath = syn::parse_quote!(Self::A::B);
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`Self :: A :: B`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`Self :: A ::`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(`Self ::`)
    "###);
    assert!(ty.path.segments.pop().is_some());
    snapshot!(ty.to_token_stream(), @r###"
    TokenStream(``)
    "###);
    assert!(ty.path.segments.pop().is_none());
}
