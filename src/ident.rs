use std::borrow::Cow;
use std::fmt::{self, Display};

#[derive(Debug, Clone, Eq, Hash, Ord, PartialOrd)]
pub struct Ident(String);

impl Ident {
    pub fn new<T: Into<Ident>>(t: T) -> Self {
        t.into()
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &str) -> Self {
        Ident(s.to_owned())
    }
}

impl<'a> From<Cow<'a, str>> for Ident {
    fn from(s: Cow<'a, str>) -> Self {
        Ident(s.into_owned())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s)
    }
}

impl From<usize> for Ident {
    fn from(u: usize) -> Self {
        Ident(u.to_string())
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Display for Ident {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(formatter)
    }
}

impl<T: ?Sized> PartialEq<T> for Ident
    where T: AsRef<str>
{
    fn eq(&self, other: &T) -> bool {
        self.0 == other.as_ref()
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::{TokenTree, TokenKind, IResult};
    #[cfg(feature = "full")]
    use lit::parsing::int;

    pub fn ident(input: &[TokenTree]) -> IResult<&[TokenTree], Ident> {
        if let IResult::Done(rest, id) = word(input) {
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
        } else {
            IResult::Error
        }
    }

    pub fn word(input: &[TokenTree]) -> IResult<&[TokenTree], Ident> {
        if let Some(&TokenTree { kind: TokenKind::Word(ref id), .. }) = input.first() {
            // Check if this word is _actually_ a lifetime, and treat that differently
            if id.chars().next().unwrap() == '\'' {
                IResult::Error
            } else {
                IResult::Done(&input[1..], Ident(id.to_string()))
            }
        } else {
            IResult::Error
        }
    }

    #[cfg(feature = "full")]
    named!(pub wordlike -> Ident, alt!(
        word
        |
        int => { |d| format!("{}", d).into() }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Ident {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(self.as_ref())
        }
    }
}
