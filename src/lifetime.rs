use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use proc_macro2::Term;
use unicode_xid::UnicodeXID;

use Span;

#[cfg_attr(feature = "extra-traits", derive(Debug))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct Lifetime {
    pub sym: Term,
    pub span: Span,
}

impl Lifetime {
    pub fn new(sym: Term, span: Span) -> Self {
        let s = sym.as_str();

        if !s.starts_with('\'') {
            panic!("lifetime name must start with apostrophe as in \"'a\", \
                   got {:?}",
                   s);
        }

        if s == "'" {
            panic!("lifetime name must not be empty");
        }

        if s == "'_" {
            panic!("\"'_\" is not a valid lifetime name");
        }

        fn xid_ok(s: &str) -> bool {
            let mut chars = s.chars();
            let first = chars.next().unwrap();
            if !(UnicodeXID::is_xid_start(first) || first == '_') {
                return false;
            }
            for ch in chars {
                if !UnicodeXID::is_xid_continue(ch) {
                    return false;
                }
            }
            true
        }

        if !xid_ok(&s[1..]) {
            panic!("{:?} is not a valid lifetime name");
        }

        Lifetime {
            sym: sym,
            span: span,
        }
    }
}

impl Display for Lifetime {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.sym.as_str().fmt(formatter)
    }
}

impl PartialEq for Lifetime {
    fn eq(&self, other: &Lifetime) -> bool {
        self.sym.as_str() == other.sym.as_str()
    }
}

impl Eq for Lifetime {}

impl PartialOrd for Lifetime {
    fn partial_cmp(&self, other: &Lifetime) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Lifetime {
    fn cmp(&self, other: &Lifetime) -> Ordering {
        self.sym.as_str().cmp(other.sym.as_str())
    }
}

impl Hash for Lifetime {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.sym.as_str().hash(h)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::{Synom, PResult, Cursor, parse_error};

    impl Synom for Lifetime {
        fn parse(input: Cursor) -> PResult<Self> {
            let (rest, span, sym) = match input.word() {
                Some(word) => word,
                _ => return parse_error(),
            };
            if !sym.as_str().starts_with('\'') {
                return parse_error();
            }

            Ok((rest, Lifetime {
                sym: sym,
                span: Span(span),
            }))
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};
    use proc_macro2::{TokenTree, TokenNode};

    impl ToTokens for Lifetime {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(TokenTree {
                span: self.span.0,
                kind: TokenNode::Term(self.sym),
            })
        }
    }
}
