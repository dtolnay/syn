#![cfg(feature = "extra-traits")]

extern crate syn;
extern crate proc_macro2;

use syn::TokenTree;
use proc_macro2::{TokenKind, OpKind, Delimiter};
use proc_macro2::Delimiter::*;

fn op(c: char) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenKind::Op(c, OpKind::Alone),
    })
}

fn delimited(delim: Delimiter, tokens: Vec<TokenTree>) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenKind::Sequence(delim, tokens.into_iter().map(|t| t.0).collect()),
    })
}

fn word(sym: &str) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenKind::Word(sym.into()),
    })
}

#[test]
fn test_struct() {
    let raw = "
        #[derive(Debug, Clone)]
        pub struct Item {
            pub ident: Ident,
            pub attrs: Vec<Attribute>,
        }
    ";

    let expected = vec![
        op('#'),
        delimited(Bracket, vec![
           word("derive"),
           delimited(Parenthesis, vec![
               word("Debug"),
               op(','),
               word("Clone"),
           ]),
        ]),
        word("pub"),
        word("struct"),
        word("Item"),
        delimited(Brace, vec![
           word("pub"),
           word("ident"),
           op(':'),
           word("Ident"),
           op(','),

           word("pub"),
           word("attrs"),
           op(':'),
           word("Vec"),
           op('<'),
           word("Attribute"),
           op('>'),
           op(','),
        ],
    )];

    let result = syn::parse_token_trees(raw.parse().unwrap()).unwrap();
    if result != expected {
        panic!("{:#?}\n!=\n{:#?}", result, expected);
    }
}
