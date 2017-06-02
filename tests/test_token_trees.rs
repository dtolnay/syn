#![cfg(feature = "extra-traits")]

extern crate syn;
extern crate proc_macro2;

use syn::TokenTree;
use proc_macro2::{TokenKind, OpKind, Delimiter, TokenStream};
use proc_macro2::Delimiter::*;

fn alone(c: char) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenKind::Op(c, OpKind::Alone),
    })
}

fn joint(c: char) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenKind::Op(c, OpKind::Joint),
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
        alone('#'),
        delimited(Bracket, vec![
           word("derive"),
           delimited(Parenthesis, vec![
               word("Debug"),
               alone(','),
               word("Clone"),
           ]),
        ]),
        word("pub"),
        word("struct"),
        word("Item"),
        delimited(Brace, vec![
           word("pub"),
           word("ident"),
           alone(':'),
           word("Ident"),
           alone(','),

           word("pub"),
           word("attrs"),
           alone(':'),
           word("Vec"),
           alone('<'),
           word("Attribute"),
           joint('>'),
           alone(','),
        ],
    )];

    let result = raw.parse::<TokenStream>().unwrap()
                    .into_iter()
                    .map(TokenTree)
                    .collect::<Vec<_>>();
    if result != expected {
        panic!("{:#?}\n!=\n{:#?}", result, expected);
    }
}
