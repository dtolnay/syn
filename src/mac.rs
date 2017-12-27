use super::*;

use proc_macro2::{TokenNode, TokenTree, Delimiter};

#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

ast_struct! {
    /// Represents a macro invocation. The Path indicates which macro
    /// is being invoked, and the vector of token-trees contains the source
    /// of the macro invocation.
    pub struct Macro #manual_extra_traits {
        pub path: Path,
        pub bang_token: Token![!],
        pub tokens: Vec<TokenTree>,
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Macro {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Macro {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
            && self.bang_token == other.bang_token
            && SliceTokenTreeHelper(&self.tokens) == SliceTokenTreeHelper(&other.tokens)
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for Macro {
    fn hash<H>(&self, state: &mut H)
        where H: Hasher
    {
        self.path.hash(state);
        self.bang_token.hash(state);
        SliceTokenTreeHelper(&self.tokens).hash(state);
    }
}

impl Macro {
    pub fn is_braced(&self) -> bool {
        match self.tokens.last() {
            Some(t) => is_braced(t),
            None => false,
        }
    }
}

pub fn is_braced(tt: &TokenTree) -> bool {
    match tt.kind {
        TokenNode::Group(Delimiter::Brace, _) => true,
        _ => false,
    }
}

#[cfg(feature = "extra-traits")]
pub struct TokenTreeHelper<'a>(pub &'a TokenTree);

#[cfg(feature = "extra-traits")]
impl<'a> PartialEq for TokenTreeHelper<'a> {
    fn eq(&self, other: &Self) -> bool {
        use proc_macro2::Spacing;

        match (&self.0.kind, &other.0.kind) {
            (&TokenNode::Group(d1, ref s1), &TokenNode::Group(d2, ref s2)) => {
                match (d1, d2) {
                    (Delimiter::Parenthesis, Delimiter::Parenthesis) |
                    (Delimiter::Brace, Delimiter::Brace) |
                    (Delimiter::Bracket, Delimiter::Bracket) |
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
                    if TokenTreeHelper(&item1) != TokenTreeHelper(&item2) {
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
impl<'a> Hash for TokenTreeHelper<'a> {
    fn hash<H: Hasher>(&self, h: &mut H) {
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

                for item in stream.clone() {
                    TokenTreeHelper(&item).hash(h);
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
pub struct SliceTokenTreeHelper<'a>(pub &'a [TokenTree]);

#[cfg(feature = "extra-traits")]
impl<'a> PartialEq for SliceTokenTreeHelper<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (a, b) in self.0.iter().zip(other.0) {
            if TokenTreeHelper(a) != TokenTreeHelper(b) {
                return false;
            }
        }
        true
    }
}

#[cfg(feature = "extra-traits")]
impl<'a> Hash for SliceTokenTreeHelper<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.len().hash(state);
        for tt in self.0 {
            TokenTreeHelper(tt).hash(state);
        }
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use proc_macro2::{TokenNode, TokenTree};
    use synom::Synom;
    use cursor::Cursor;
    use {PResult, parse_error};

    impl Synom for Macro {
        named!(parse -> Self, do_parse!(
            what: syn!(Path) >>
            bang: punct!(!) >>
            body: call!(parse_tt_delimited) >>
            (Macro {
                path: what,
                bang_token: bang,
                tokens: vec![body],
            })
        ));
    }

    pub fn parse_tt_list(input: Cursor) -> PResult<Vec<TokenTree>> {
        Ok((Cursor::empty(), input.token_stream().into_iter().collect()))
    }

    pub fn parse_tt_delimited(input: Cursor) -> PResult<TokenTree> {
        match input.token_tree() {
            Some((rest, token @ TokenTree { kind: TokenNode::Group(..), .. })) => {
                Ok((rest, token))
            }
            _ => parse_error(),
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Macro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.bang_token.to_tokens(tokens);
            tokens.append_all(&self.tokens);
        }
    }
}
