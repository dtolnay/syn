use std::fmt::{self, Display};

#[derive(Debug, Clone, Eq, PartialEq)]
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

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s)
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Inherited,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    fn ident_ch(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    named!(pub word<&str, Ident>, preceded!(
        opt!(call!(::nom::multispace)),
        map!(take_while1_s!(ident_ch), Into::into)
    ));

    named!(pub visibility<&str, Visibility>, preceded!(
        opt!(call!(::nom::multispace)),
        alt!(
            terminated!(tag_s!("pub"), call!(::nom::multispace)) => { |_| Visibility::Public }
            |
            epsilon!() => { |_| Visibility::Inherited }
        )
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
