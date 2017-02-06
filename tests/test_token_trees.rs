extern crate syn;
use syn::{TokenTree, Span, EMPTY_SPAN};
use syn::DelimToken::*;
use syn::Token::*;
use syn::fold::Folder;

struct StripSpans;
impl Folder for StripSpans {
    fn fold_span(&mut self, span: Span) -> Span {
        assert!(span != EMPTY_SPAN);
        EMPTY_SPAN
    }
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
        token(Pound),
        delimited(Bracket, vec![
            ident("derive"),
            delimited(Paren, vec![
                ident("Debug"),
                token(Comma),
                ident("Clone"),
            ]),
        ]),
        ident("pub"),
        ident("struct"),
        ident("Item"),
        delimited(Brace, vec![
            ident("pub"),
            ident("ident"),
            token(Colon),
            ident("Ident"),
            token(Comma),

            ident("pub"),
            ident("attrs"),
            token(Colon),
            ident("Vec"),
            token(Lt),
            ident("Attribute"),
            token(Gt),
            token(Comma),
        ]),
    ];

    let mut result = syn::parse_token_trees(raw).unwrap();
    for tt in &mut result {
        *tt = StripSpans.fold_tt(tt.clone());
    }
    if result != expected {
        panic!("{:#?}\n!=\n{:#?}", result, expected);
    }
}

fn delimited(delim: syn::DelimToken, tts: Vec<TokenTree>) -> TokenTree {
    TokenTree::Delimited(syn::Delimited {
        delim: delim,
        tts: tts,
    }, EMPTY_SPAN)
}

fn ident(s: &str) -> TokenTree {
    TokenTree::Token(Ident(syn::Ident::new(s)), EMPTY_SPAN)
}

fn token(token: syn::Token) -> TokenTree {
    TokenTree::Token(token, EMPTY_SPAN)
}
