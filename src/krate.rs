use super::*;

ast_struct! {
    pub struct Crate {
        pub shebang: Option<String>,
        pub attrs: Vec<Attribute>,
        pub items: Vec<Item>,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::{IResult, Synom, ParseError};
    use proc_macro2::TokenTree;

    impl Synom for Crate {
        named!(parse -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_inner)) >>
            items: many0!(syn!(Item)) >>
            (Crate {
                shebang: None,
                attrs: attrs,
                items: items,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("crate")
        }

        fn parse_str_all(mut input: &str) -> Result<Self, ParseError> {
            // Strip the BOM if it is present
            const BOM: &'static str = "\u{feff}";
            if input.starts_with(BOM) {
                input = &input[BOM.len()..];
            }

            let mut shebang = None;
            if input.starts_with("#!") && !input.starts_with("#![") {
                if let Some(idx) = input.find('\n') {
                    shebang = Some(input[..idx].to_string());
                    input = &input[idx..];
                } else {
                    shebang = Some(input.to_string());
                    input = "";
                }
            }

            let mut krate: Crate = Self::parse_all(input.parse()?)?;
            krate.shebang = shebang;
            Ok(krate)
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Crate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
        }
    }
}
