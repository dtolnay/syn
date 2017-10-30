use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use proc_macro2::Term;
use unicode_xid::UnicodeXID;

use Span;
use tokens;

/// A word of Rust code, such as a keyword or variable name.
///
/// An identifier consists of at least one Unicode code point, the first of
/// which has the XID_Start property and the rest of which have the XID_Continue
/// property. An underscore may be used as the first character as long as it is
/// not the only character.
///
/// - The empty string is not an identifier. Use `Option<Ident>`.
/// - An underscore by itself is not an identifier. Use
///   `syn::tokens::Underscore` instead.
/// - A lifetime is not an identifier. Use `syn::Lifetime` instead.
///
/// An identifier constructed with `Ident::new` is permitted to be a Rust
/// keyword, though parsing input with [`parse`], [`parse_str`] or
/// [`parse_tokens`] rejects Rust keywords.
///
/// [`parse`]: fn.parse.html
/// [`parse_str`]: fn.parse_str.html
/// [`parse_tokens`]: fn.parse_tokens.html
///
/// # Examples
///
/// A new ident can be created from a string using the `Ident::from` function.
///
/// ```rust
/// extern crate syn;
/// use syn::Ident;
/// #
/// # fn main() {
///
/// let ident = Ident::from("another_identifier");
///
/// # }
/// ```
///
/// When the ident is used in Macros 1.1 output, it needs to be turned into
/// a token stream. This is easy to do using the `quote!` macro from the `quote`
/// crate.
///
/// ```rust
/// # #[macro_use]
/// # extern crate quote;
/// # extern crate syn;
/// # use syn::Ident;
/// # fn main() {
/// # let ident = Ident::from("another_identifier");
/// #
/// // Create tokens using the ident.
/// let expanded = quote! { let #ident = 10; };
///
/// // Derive a new ident from the existing one.
/// let temp_ident = Ident::from(format!("new_{}", ident));
/// let expanded = quote! { let $temp_ident = 10; };
///
/// # }
/// ```
///
/// If `syn` is used to parse existing Rust source code, it is often useful to
/// convert the `Ident` to a more generic string data type at some point. The
/// methods `as_ref()` and `to_string()` achieve this.
/// 
/// ```rust
/// # use syn::Ident;
/// # let ident = Ident::from("another_identifier");
/// #
/// // Examine the ident as a &str.
/// let ident_str = ident.as_ref();
/// if ident_str.len() > 60 {
///     println!("Very long identifier: {}", ident_str)
/// }
///
/// // Create a String from the ident.
/// let ident_string = ident.to_string();
/// give_away(ident_string);
///
/// fn give_away(s: String) { /* ... */ }
/// ```
#[derive(Copy, Clone, Debug)]
pub struct Ident {
    pub sym: Term,
    pub span: Span,
}

impl Ident {

    /// Creates a new `Ident` from the structured items. This is mainly used
    /// by the parser to create `Ident`s from existing Rust source code.
    ///
    /// Creating new `Ident`s programmatically is easier with `Ident::from`.
    pub fn new(sym: Term, span: Span) -> Self {
        let s = sym.as_str();

        if s.is_empty() {
            panic!("ident is not allowed to be empty; use Option<Ident>");
        }

        if s.starts_with('\'') {
            panic!("ident is not allowed to be a lifetime; use syn::Lifetime");
        }

        if s == "_" {
            panic!("`_` is not a valid ident; use syn::tokens::Underscore");
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

        fn integer_ok(s: &str) -> bool {
            s.bytes().all(|digit| digit >= b'0' && digit <= b'9')
        }

        if !(xid_ok(s) || integer_ok(s)) {
            panic!("{:?} is not a valid ident", s);
        }

        Ident {
            sym: sym,
            span: span,
        }
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &str) -> Self {
        Ident::new(Term::intern(s), Span::default())
    }
}

impl From<tokens::Self_> for Ident {
    fn from(tok: tokens::Self_) -> Self {
        Ident::new(Term::intern("self"), tok.0)
    }
}

impl From<tokens::CapSelf> for Ident {
    fn from(tok: tokens::CapSelf) -> Self {
        Ident::new(Term::intern("Self"), tok.0)
    }
}

impl From<tokens::Super> for Ident {
    fn from(tok: tokens::Super) -> Self {
        Ident::new(Term::intern("super"), tok.0)
    }
}

impl<'a> From<Cow<'a, str>> for Ident {
    fn from(s: Cow<'a, str>) -> Self {
        Ident::new(Term::intern(&s), Span::default())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident::new(Term::intern(&s), Span::default())
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
    use proc_macro2::{TokenTree, TokenNode};

    impl ToTokens for Ident {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(TokenTree {
                span: self.span.0,
                kind: TokenNode::Term(self.sym),
            })
        }
    }
}
