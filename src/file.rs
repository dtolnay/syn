use super::*;

ast_struct! {
    pub struct File {
        pub shebang: Option<String>,
        pub attrs: Vec<Attribute>,
        pub items: Vec<Item>,
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::Synom;

    impl Synom for File {
        named!(parse -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_inner)) >>
            items: many0!(syn!(Item)) >>
            (File {
                shebang: None,
                attrs: attrs,
                items: items,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("crate")
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use quote::{Tokens, ToTokens};

    impl ToTokens for File {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
        }
    }
}
