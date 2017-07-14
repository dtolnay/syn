use super::*;
use delimited::Delimited;

ast_struct! {
    /// An enum variant.
    pub struct Variant {
        /// Name of the variant.
        pub ident: Ident,

        /// Attributes tagged on the variant.
        pub attrs: Vec<Attribute>,

        /// Type of variant.
        pub data: VariantData,

        /// Explicit discriminant, e.g. `Foo = 1`
        pub discriminant: Option<Expr>,

        pub eq_token: Option<tokens::Eq>,
    }
}

ast_enum! {
    /// Data stored within an enum variant or struct.
    pub enum VariantData {
        /// Struct variant, e.g. `Point { x: f64, y: f64 }`.
        Struct(Delimited<Field, tokens::Comma>, tokens::Brace),

        /// Tuple variant, e.g. `Some(T)`.
        Tuple(Delimited<Field, tokens::Comma>, tokens::Paren),

        /// Unit variant, e.g. `None`.
        Unit,
    }
}

impl VariantData {
    // TODO: expose this?
    // /// Slice containing the fields stored in the variant.
    // pub fn fields(&self) -> &Delimited<Field, tokens::Comma> {
    //     match *self {
    //         VariantData::Struct(ref fields, _) |
    //         VariantData::Tuple(ref fields, _) => fields,
    //         VariantData::Unit => &[],
    //     }
    // }
    //
    // /// Mutable slice containing the fields stored in the variant.
    // pub fn fields_mut(&mut self) -> &mut Delimited<Field, tokens::Comma> {
    //     match *self {
    //         VariantData::Struct(ref mut fields, _) |
    //         VariantData::Tuple(ref mut fields, _) => fields,
    //         VariantData::Unit => &mut [],
    //     }
    // }
}

ast_struct! {
    /// A field of a struct or enum variant.
    pub struct Field {
        /// Name of the field, if any.
        ///
        /// Fields of tuple structs have no names.
        pub ident: Option<Ident>,

        /// Visibility of the field.
        pub vis: Visibility,

        /// Attributes tagged on the field.
        pub attrs: Vec<Attribute>,

        /// Type of the field.
        pub ty: Ty,

        pub colon_token: Option<tokens::Colon>,
    }
}

ast_enum_of_structs! {
    /// Visibility level of an item.
    pub enum Visibility {
        /// Public, i.e. `pub`.
        pub Public(VisPublic {
            pub pub_token: tokens::Pub,
        }),

        /// Crate-visible, i.e. `pub(crate)`.
        pub Crate(VisCrate {
            pub pub_token: tokens::Pub,
            pub paren_token: tokens::Paren,
            pub crate_token: tokens::Crate,
        }),

        /// Restricted, e.g. `pub(self)` or `pub(super)` or `pub(in some::module)`.
        pub Restricted(VisRestricted {
            pub pub_token: tokens::Pub,
            pub paren_token: tokens::Paren,
            pub in_token: Option<tokens::In>,
            pub path: Box<Path>,
        }),

        /// Inherited, i.e. private.
        pub Inherited(VisInherited {}),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::Synom;
    use synom::tokens;
    use synom::tokens::*;

    impl Field {
        named!(pub parse_struct -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_outer)) >>
            vis: syn!(Visibility) >>
            id: syn!(Ident) >>
            colon: syn!(Colon) >>
            ty: syn!(Ty) >>
            (Field {
                ident: Some(id),
                vis: vis,
                attrs: attrs,
                ty: ty,
                colon_token: Some(colon),
            })
        ));

        named!(pub parse_tuple -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_outer)) >>
            vis: syn!(Visibility) >>
            ty: syn!(Ty) >>
            (Field {
                ident: None,
                colon_token: None,
                vis: vis,
                attrs: attrs,
                ty: ty,
            })
        ));
    }

    impl Synom for Visibility {
        named!(parse -> Self, alt!(
            do_parse!(
                pub_token: syn!(Pub) >>
                other: parens!(syn!(tokens::Crate)) >>
                (Visibility::Crate(VisCrate {
                    crate_token: other.0,
                    paren_token: other.1,
                    pub_token: pub_token,
                }))
            )
            |
            do_parse!(
                pub_token: syn!(Pub) >>
                other: parens!(syn!(Self_)) >>
                (Visibility::Restricted(VisRestricted {
                    path: Box::new(other.0.into()),
                    in_token: None,
                    paren_token: other.1,
                    pub_token: pub_token,
                }))
            )
            |
            do_parse!(
                pub_token: syn!(Pub) >>
                other: parens!(syn!(Super)) >>
                (Visibility::Restricted(VisRestricted {
                    path: Box::new(other.0.into()),
                    in_token: None,
                    paren_token: other.1,
                    pub_token: pub_token,
                }))
            )
            |
            do_parse!(
                pub_token: syn!(Pub) >>
                other: parens!(do_parse!(
                    in_tok: syn!(In) >>
                    restricted: call!(Path::parse_mod_style) >>
                    (in_tok, restricted)
                )) >>
                (Visibility::Restricted(VisRestricted {
                    path: Box::new((other.0).1),
                    in_token: Some((other.0).0),
                    paren_token: other.1,
                    pub_token: pub_token,
                }))
            )
            |
            syn!(Pub) => { |tok| {
                Visibility::Public(VisPublic {
                    pub_token: tok,
                })
            } }
            |
            epsilon!() => { |_| Visibility::Inherited(VisInherited {}) }
        ));
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Variant {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(&self.attrs);
            self.ident.to_tokens(tokens);
            self.data.to_tokens(tokens);
            if let Some(ref disc) = self.discriminant {
                TokensOrDefault(&self.eq_token).to_tokens(tokens);
                disc.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for VariantData {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                VariantData::Struct(ref fields, ref brace) => {
                    brace.surround(tokens, |tokens| {
                        fields.to_tokens(tokens);
                    });
                }
                VariantData::Tuple(ref fields, ref paren) => {
                    paren.surround(tokens, |tokens| {
                        fields.to_tokens(tokens);
                    });
                }
                VariantData::Unit => {}
            }
        }
    }

    impl ToTokens for Field {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            if let Some(ref ident) = self.ident {
                ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for VisPublic {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pub_token.to_tokens(tokens)
        }
    }

    impl ToTokens for VisCrate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pub_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.crate_token.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for VisRestricted {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pub_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                // XXX: If we have a path which is not "self" or "super",
                // automatically add the "in" token.
                self.in_token.to_tokens(tokens);
                self.path.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for VisInherited {
        fn to_tokens(&self, _tokens: &mut Tokens) {
        }
    }
}
