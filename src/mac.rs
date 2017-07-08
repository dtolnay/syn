#[cfg(feature = "extra-traits")]
use std::fmt;

use super::*;

use proc_macro2::{TokenNode, Delimiter};

ast_struct! {
    /// Represents a macro invocation. The Path indicates which macro
    /// is being invoked, and the vector of token-trees contains the source
    /// of the macro invocation.
    pub struct Mac {
        pub path: Path,
        pub bang_token: tokens::Bang,
        /// The `example` in `macro_rules! example { ... }`.
        pub ident: Option<Ident>,
        pub tokens: Vec<TokenTree>,
    }
}

#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct TokenTree(pub proc_macro2::TokenTree);

impl Mac {
    pub fn is_braced(&self) -> bool {
        match self.tokens.last() {
            Some(t) => t.is_braced(),
            None => false,
        }
    }
}

impl TokenTree {
    pub fn is_braced(&self) -> bool {
        match self.0.kind {
            TokenNode::Group(Delimiter::Brace, _) => true,
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl PartialEq for TokenTree {
    fn eq(&self, other: &TokenTree) -> bool {
        use proc_macro2::Spacing;

        match (&self.0.kind, &other.0.kind) {
            (&TokenNode::Group(d1, ref s1), &TokenNode::Group(d2, ref s2)) => {
                match (d1, d2) {
                    (Delimiter::Parenthesis, Delimiter::Parenthesis) |
                    (Delimiter::Brace, Delimiter::Brace) |
                    (Delimiter::Bracket, Delimiter::Bracket) => {}
                    (Delimiter::None, Delimiter::None) => {}
                    _ => return false,
                }

                let s1 = s1.clone().into_iter();
                let mut s2 = s2.clone().into_iter();

                for item1 in s1 {
                    let item2 = match s2.next() {
                        Some(item) => item,
                        None => return false,
                    };
                    if TokenTree(item1) != TokenTree(item2) {
                        return false
                    }
                }
                s2.next().is_none()
            }
            (&TokenNode::Op(o1, k1), &TokenNode::Op(o2, k2)) => {
                o1 == o2 && match (k1, k2) {
                    (Spacing::Alone, Spacing::Alone) |
                    (Spacing::Joint, Spacing::Joint) => true,
                    _ => false,
                }
            }
            (&TokenNode::Literal(ref l1), &TokenNode::Literal(ref l2)) => {
                l1.to_string() == l2.to_string()
            }
            (&TokenNode::Term(ref s1), &TokenNode::Term(ref s2)) => {
                s1.as_str() == s2.as_str()
            }
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for TokenTree {}

#[cfg(feature = "extra-traits")]
impl ::std::hash::Hash for TokenTree {
    fn hash<H: ::std::hash::Hasher>(&self, h: &mut H) {
        use proc_macro2::Spacing;

        match self.0.kind {
            TokenNode::Group(delim, ref stream) => {
                0u8.hash(h);
                match delim {
                    Delimiter::Parenthesis => 0u8.hash(h),
                    Delimiter::Brace => 1u8.hash(h),
                    Delimiter::Bracket => 2u8.hash(h),
                    Delimiter::None => 3u8.hash(h),
                }

                for item in stream.clone().into_iter() {
                    TokenTree(item).hash(h);
                }
                0xffu8.hash(h); // terminator w/ a variant we don't normally hash
            }
            TokenNode::Op(op, kind) => {
                1u8.hash(h);
                op.hash(h);
                match kind {
                    Spacing::Alone => 0u8.hash(h),
                    Spacing::Joint => 1u8.hash(h),
                }
            }
            TokenNode::Literal(ref lit) => (2u8, lit.to_string()).hash(h),
            TokenNode::Term(ref word) => (3u8, word.as_str()).hash(h),
        }
    }
}

#[cfg(feature = "extra-traits")]
impl fmt::Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.to_string().fmt(f)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use proc_macro2::{TokenNode, TokenTree};
    use synom::tokens::*;
    use synom::{Synom, PResult, Cursor, parse_error};

    impl Synom for Mac {
        named!(parse -> Self, do_parse!(
            what: syn!(Path) >>
            bang: syn!(Bang) >>
            body: call!(::TokenTree::parse_delimited) >>
            (Mac {
                path: what,
                bang_token: bang,
                ident: None,
                tokens: vec![body],
            })
        ));
    }

    impl ::TokenTree {
        pub fn parse_list(input: Cursor) -> PResult<Vec<Self>> {
            Ok((Cursor::empty(), input.token_stream().into_iter().map(::TokenTree).collect()))
        }

        pub fn parse_delimited(input: Cursor) -> PResult<Self> {
            match input.token_tree() {
                Some((rest, token @ TokenTree { kind: TokenNode::Group(..), .. })) => {
                    Ok((rest, ::TokenTree(token)))
                }
                _ => parse_error(),
            }
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Mac {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.bang_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            tokens.append_all(&self.tokens);
        }
    }

    impl ToTokens for TokenTree {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.to_tokens(tokens);
        }
    }
}
