use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use proc_macro2::Symbol;

use Span;
use tokens;

#[derive(Clone, Debug)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(sym: Symbol, span: Span) -> Self {
        Ident {
            sym: sym,
            span: span,
        }
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &str) -> Self {
        Ident::new(s.into(), Span::default())
    }
}

impl From<tokens::Self_> for Ident {
    fn from(tok: tokens::Self_) -> Self {
        Ident::new("self".into(), tok.0)
    }
}

impl From<tokens::CapSelf> for Ident {
    fn from(tok: tokens::CapSelf) -> Self {
        Ident::new("Self".into(), tok.0)
    }
}

impl From<tokens::Super> for Ident {
    fn from(tok: tokens::Super) -> Self {
        Ident::new("super".into(), tok.0)
    }
}

impl<'a> From<Cow<'a, str>> for Ident {
    fn from(s: Cow<'a, str>) -> Self {
        Ident::new(s[..].into(), Span::default())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident::new(s[..].into(), Span::default())
    }
}

impl From<usize> for Ident {
    fn from(u: usize) -> Self {
        Ident::new(u.to_string()[..].into(), Span::default())
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.sym.as_str()
    }
}

impl Display for Ident {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.sym.as_str().fmt(formatter)
    }
}

impl<T: ?Sized> PartialEq<T> for Ident
    where T: AsRef<str>
{
    fn eq(&self, other: &T) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Ident) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Ident) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_ref().hash(h)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::{Synom, PResult, Cursor, parse_error};

    impl Synom for Ident {
        fn parse(input: Cursor) -> PResult<Self> {
            let (rest, span, sym) = match input.word() {
                Some(word) => word,
                _ => return parse_error(),
            };
            if sym.as_str().starts_with('\'') {
                return parse_error();
            }
            match sym.as_str() {
                // From https://doc.rust-lang.org/grammar.html#keywords
                "abstract" | "alignof" | "as" | "become" | "box" | "break" | "const" | "continue" |
                "crate" | "do" | "else" | "enum" | "extern" | "false" | "final" | "fn" | "for" |
                "if" | "impl" | "in" | "let" | "loop" | "macro" | "match" | "mod" | "move" |
                "mut" | "offsetof" | "override" | "priv" | "proc" | "pub" | "pure" | "ref" |
                "return" | "Self" | "self" | "sizeof" | "static" | "struct" | "super" | "trait" |
                "true" | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" |
                "while" | "yield" => return parse_error(),
                _ => {}
            }

            Ok((rest, Ident {
                span: Span(span),
                sym: sym,
            }))
        }

        fn description() -> Option<&'static str> {
            Some("identifier")
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};
    use proc_macro2::{TokenTree, TokenKind};

    impl ToTokens for Ident {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(TokenTree {
                span: self.span.0,
                kind: TokenKind::Word(self.sym),
            })
        }
    }
}
