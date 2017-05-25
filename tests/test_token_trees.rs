extern crate syn;
use syn::TokenTree::{self, Token};
use syn::DelimToken::*;
use syn::Token::*;

#[test]
fn test_struct() {
    let raw = "
        #[derive(Debug, Clone)]
        pub struct Item {
            pub ident: Ident,
            pub attrs: Vec<Attribute>,
        }
    ";

    let expected =
        vec![Token(Pound),
             delimited(Bracket,
                       vec![ident("derive"),
                            delimited(Paren, vec![ident("Debug"), Token(Comma), ident("Clone")])]),
             ident("pub"),
             ident("struct"),
             ident("Item"),
             delimited(Brace,
                       vec![ident("pub"),
                            ident("ident"),
                            Token(Colon),
                            ident("Ident"),
                            Token(Comma),

                            ident("pub"),
                            ident("attrs"),
                            Token(Colon),
                            ident("Vec"),
                            Token(Lt),
                            ident("Attribute"),
                            Token(Gt),
                            Token(Comma)])];

    let result = syn::parse_token_trees(raw.parse().unwrap()).unwrap();
    if result != expected {
        panic!("{:#?}\n!=\n{:#?}", result, expected);
    }
}

fn delimited(delim: syn::DelimToken, tts: Vec<TokenTree>) -> TokenTree {
    TokenTree::Delimited(syn::Delimited {
                             delim: delim,
                             tts: tts,
                         })
}

fn ident(s: &str) -> TokenTree {
    TokenTree::Token(Ident(syn::Ident::new(s)))
}
