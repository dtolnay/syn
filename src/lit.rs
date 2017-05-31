use std::fmt;
use std::hash::{Hash, Hasher};

use proc_macro2::{self, Literal, TokenKind};

use {Span, TokenTree};

#[derive(Clone)]
pub struct Lit {
    pub value: LitKind,
    pub span: Span,
}

#[derive(Clone)]
pub enum LitKind {
    Bool(bool),
    Other(Literal),
}

impl Lit {
    pub fn into_token_tree(self) -> TokenTree {
        let kind = match self.value {
            LitKind::Bool(true) => TokenKind::Word("true".into()),
            LitKind::Bool(false) => TokenKind::Word("false".into()),
            LitKind::Other(l) => TokenKind::Literal(l),
        };
        TokenTree(proc_macro2::TokenTree {
            span: self.span.0,
            kind: kind,
        })
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl PartialEq for Lit {
    fn eq(&self, other: &Lit) -> bool {
        self.value == other.value
    }
}

impl Eq for Lit {}

impl Hash for Lit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.value.hash(hasher)
    }
}

impl fmt::Display for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitKind::Bool(b) => b.fmt(f),
            LitKind::Other(ref l) => l.fmt(f),
        }
    }
}

impl fmt::Debug for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitKind::Bool(b) => b.fmt(f),
            LitKind::Other(ref l) => fmt::Display::fmt(l, f),
        }
    }
}

impl PartialEq for LitKind {
    fn eq(&self, other: &LitKind) -> bool {
        match (self, other) {
            (&LitKind::Bool(b1), &LitKind::Bool(b2)) => b1 == b2,
            (&LitKind::Other(ref l1), &LitKind::Other(ref l2)) => {
                l1.to_string() == l2.to_string()
            }
            _ => false,
        }
    }
}

impl Eq for LitKind {}

impl Hash for LitKind {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match *self {
            LitKind::Bool(b) => (0u8, b).hash(hasher),
            LitKind::Other(ref l) => (1u8, l.to_string()).hash(hasher),
        }
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use proc_macro2::TokenTree;
    use synom::{Synom, IResult};

    impl Synom for Lit {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            let mut tokens = input.iter();
            let token = match tokens.next() {
                Some(token) => token,
                None => return IResult::Error,
            };
            let kind = match token.kind {
                TokenKind::Literal(ref l) => LitKind::Other(l.clone()),
                TokenKind::Word(ref s) if s.as_str() == "true" => LitKind::Bool(true),
                TokenKind::Word(ref s) if s.as_str() == "false" => LitKind::Bool(false),
                _ => return IResult::Error,
            };
            IResult::Done(tokens.as_slice(), Lit {
                span: Span(token.span),
                value: kind,
            })
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    use proc_macro2::{TokenTree, TokenKind};

    impl ToTokens for Lit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let kind = match self.value {
                LitKind::Bool(true) => TokenKind::Word("true".into()),
                LitKind::Bool(false) => TokenKind::Word("false".into()),
                LitKind::Other(ref l) => TokenKind::Literal(l.clone()),
            };
            tokens.append(TokenTree {
                span: self.span.0,
                kind: kind,
            });
        }
    }
}
