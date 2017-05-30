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
        pub discriminant: Option<ConstExpr>,

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
    use WhereClause;
    #[cfg(feature = "full")]
    use ConstExpr;
    use attr::parsing::outer_attr;
    #[cfg(feature = "full")]
    use constant::parsing::const_expr;
    #[cfg(feature = "full")]
    use expr::parsing::expr;
    use generics::parsing::where_clause;
    use ident::parsing::ident;
    use ty::parsing::{mod_style_path, ty};

    named!(pub struct_body -> (WhereClause, VariantData, Option<tokens::Semi>), alt!(
        do_parse!(
            wh: where_clause >>
            body: struct_like_body >>
            (wh, VariantData::Struct(body.0, body.1), None)
        )
        |
        do_parse!(
            body: tuple_like_body >>
            wh: where_clause >>
            punct!(";") >>
            (wh, VariantData::Tuple(body.0, body.1), Some(tokens::Semi::default()))
        )
        |
        do_parse!(
            wh: where_clause >>
            punct!(";") >>
            (wh, VariantData::Unit, Some(tokens::Semi::default()))
        )
    ));

    named!(pub enum_body -> (WhereClause, Delimited<Variant, tokens::Comma>, tokens::Brace), do_parse!(
        wh: where_clause >>
        punct!("{") >>
        variants: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                   variant) >>
        punct!("}") >>
        (wh, variants, tokens::Brace::default())
    ));

    named!(variant -> Variant, do_parse!(
        attrs: many0!(outer_attr) >>
        id: ident >>
        data: alt!(
            struct_like_body => { |(d, b)| VariantData::Struct(d, b) }
            |
            tuple_like_body => { |(d, b)| VariantData::Tuple(d, b) }
            |
            epsilon!() => { |_| VariantData::Unit }
        ) >>
        disr: option!(preceded!(punct!("="), discriminant)) >>
        (Variant {
            ident: id,
            attrs: attrs,
            data: data,
            eq_token: disr.as_ref().map(|_| tokens::Eq::default()),
            discriminant: disr,
        })
    ));

    #[cfg(not(feature = "full"))]
    use constant::parsing::const_expr as discriminant;

    #[cfg(feature = "full")]
    named!(discriminant -> ConstExpr, alt!(
        terminated!(const_expr, after_discriminant)
        |
        terminated!(expr, after_discriminant) => { ConstExpr::Other }
    ));

    // XXX: HACKY
    #[cfg(feature = "full")]
    pub fn eof(input: &[synom::TokenTree]) -> synom::IResult<&[synom::TokenTree], &'static str> {
        if input.is_empty() {
            synom::IResult::Done(&[], "")
        } else {
            synom::IResult::Error
        }
    }

    #[cfg(feature = "full")]
    named!(after_discriminant -> &str, peek!(alt!(punct!(",") | input_end!())));

    named!(pub struct_like_body -> (Delimited<Field, tokens::Comma>, tokens::Brace), do_parse!(
        punct!("{") >>
        fields: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 struct_field) >>
        punct!("}") >>
        (fields, tokens::Brace::default())
    ));

    named!(tuple_like_body -> (Delimited<Field, tokens::Comma>, tokens::Paren), do_parse!(
        punct!("(") >>
        fields: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 tuple_field) >>
        punct!(")") >>
        (fields, tokens::Paren::default())
    ));

    named!(struct_field -> Field, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        (Field {
            ident: Some(id),
            vis: vis,
            attrs: attrs,
            ty: ty,
            colon_token: Some(tokens::Colon::default()),
        })
    ));

    named!(tuple_field -> Field, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        ty: ty >>
        (Field {
            ident: None,
            colon_token: None,
            vis: vis,
            attrs: attrs,
            ty: ty,
        })
    ));

    named!(pub visibility -> Visibility, alt!(
        do_parse!(
            keyword!("pub") >>
            punct!("(") >>
            keyword!("crate") >>
            punct!(")") >>
            (Visibility::Crate(VisCrate {
                crate_token: tokens::Crate::default(),
                paren_token: tokens::Paren::default(),
                pub_token: tokens::Pub::default(),
            }))
        )
        |
        do_parse!(
            keyword!("pub") >>
            punct!("(") >>
            keyword!("self") >>
            punct!(")") >>
            (Visibility::Restricted(VisRestricted {
                path: Box::new("self".into()),
                in_token: None,
                paren_token: tokens::Paren::default(),
                pub_token: tokens::Pub::default(),
            }))
        )
        |
        do_parse!(
            keyword!("pub") >>
            punct!("(") >>
            keyword!("super") >>
            punct!(")") >>
            (Visibility::Restricted(VisRestricted {
                path: Box::new("super".into()),
                in_token: None,
                paren_token: tokens::Paren::default(),
                pub_token: tokens::Pub::default(),
            }))
        )
        |
        do_parse!(
            keyword!("pub") >>
            punct!("(") >>
            keyword!("in") >>
            restricted: mod_style_path >>
            punct!(")") >>
            (Visibility::Restricted(VisRestricted {
                path: Box::new(restricted),
                in_token: Some(tokens::In::default()),
                paren_token: tokens::Paren::default(),
                pub_token: tokens::Pub::default(),
            }))
        )
        |
        keyword!("pub") => { |_| {
            Visibility::Public(VisPublic {
                pub_token: tokens::Pub::default(),
            })
        } }
        |
        epsilon!() => { |_| Visibility::Inherited(VisInherited {}) }
    ));
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
            self.eq_token.to_tokens(tokens);
            self.discriminant.to_tokens(tokens);
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
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
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
