// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;

ast_struct! {
    /// A complete file of Rust source code.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
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
            attrs: many0!(Attribute::parse_inner) >>
            items: many0!(Item::parse) >>
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
    use quote::{ToTokens, Tokens};

    impl ToTokens for File {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.inner());
            tokens.append_all(&self.items);
        }
    }
}
