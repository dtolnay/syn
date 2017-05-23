use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use proc_macro2::Symbol;

use Span;

#[derive(Clone)]
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

impl fmt::Debug for Ident {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.sym.as_str(), formatter)
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
    use synom::IResult;
    use synom::space::skip_whitespace;
    use unicode_xid::UnicodeXID;

    pub fn ident(input: &str) -> IResult<&str, Ident> {
        let (rest, id) = match word(input) {
            IResult::Done(rest, id) => (rest, id),
            IResult::Error => return IResult::Error,
        };

        match id.as_ref() {
            // From https://doc.rust-lang.org/grammar.html#keywords
            "abstract" | "alignof" | "as" | "become" | "box" | "break" | "const" | "continue" |
            "crate" | "do" | "else" | "enum" | "extern" | "false" | "final" | "fn" | "for" |
            "if" | "impl" | "in" | "let" | "loop" | "macro" | "match" | "mod" | "move" |
            "mut" | "offsetof" | "override" | "priv" | "proc" | "pub" | "pure" | "ref" |
            "return" | "Self" | "self" | "sizeof" | "static" | "struct" | "super" | "trait" |
            "true" | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" |
            "while" | "yield" => IResult::Error,
            _ => IResult::Done(rest, id),
        }
    }

    pub fn word(mut input: &str) -> IResult<&str, Ident> {
        input = skip_whitespace(input);

        let mut chars = input.char_indices();
        match chars.next() {
            Some((_, ch)) if UnicodeXID::is_xid_start(ch) || ch == '_' => {}
            _ => return IResult::Error,
        }

        for (i, ch) in chars {
            if !UnicodeXID::is_xid_continue(ch) {
                return IResult::Done(&input[i..], input[..i].into());
            }
        }

        IResult::Done("", input.into())
    }

    #[cfg(feature = "full")]
    pub fn wordlike(mut input: &str) -> IResult<&str, Ident> {
        input = skip_whitespace(input);

        for (i, ch) in input.char_indices() {
            if !UnicodeXID::is_xid_start(ch) && !UnicodeXID::is_xid_continue(ch) {
                return if i == 0 {
                           IResult::Error
                       } else {
                           IResult::Done(&input[i..], input[..i].into())
                       };
            }
        }

        IResult::Done("", input.into())
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
