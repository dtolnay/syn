#[macro_use]
mod macros;

use std::str::FromStr;

use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenStream, TokenTree};
use quote::quote;
use std::iter::FromIterator;
use syn::{Expr, ExprRange};

#[test]
fn test_expr_parse() {
    let code = "..100u32";
    let tt = TokenStream::from_str(code).unwrap();
    let expr: Expr = syn::parse2(tt.clone()).unwrap();
    let expr_range: ExprRange = syn::parse2(tt).unwrap();
    assert_eq!(expr, Expr::Range(expr_range));
}

#[test]
fn test_await() {
    // Must not parse as Expr::Field.
    let expr = syn::parse_str::<Expr>("fut.await").unwrap();

    snapshot!(expr, @r###"
    Expr::Await {
        base: Expr::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "fut",
                        arguments: None,
                    },
                ],
            },
        },
    }
    "###);
}

#[test]
fn test_macro_variable_func() {
    // mimics the token stream corresponding to `$fn()`
    let tokens = TokenStream::from_iter(vec![
        TokenTree::Group(Group::new(Delimiter::None, quote! { f })),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
    ]);

    snapshot!(tokens as Expr, @r###"
    Expr::Call {
        func: Expr::Group {
            expr: Expr::Path {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "f",
                            arguments: None,
                        },
                    ],
                },
            },
        },
    }
    "###);

    let tokens = TokenStream::from_iter(vec![
        TokenTree::Punct(Punct::new('#', Spacing::Alone)),
        TokenTree::Group(Group::new(Delimiter::Bracket, quote! { outside })),
        TokenTree::Group(Group::new(Delimiter::None, quote! { #[inside] f })),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
    ]);

    snapshot!(tokens as Expr, @r###"
    Expr::Call {
        attrs: [
            Attribute {
                style: Outer,
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "outside",
                            arguments: None,
                        },
                    ],
                },
                tokens: TokenStream(``),
            },
        ],
        func: Expr::Group {
            expr: Expr::Path {
                attrs: [
                    Attribute {
                        style: Outer,
                        path: Path {
                            segments: [
                                PathSegment {
                                    ident: "inside",
                                    arguments: None,
                                },
                            ],
                        },
                        tokens: TokenStream(``),
                    },
                ],
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "f",
                            arguments: None,
                        },
                    ],
                },
            },
        },
    }
    "###);
}
