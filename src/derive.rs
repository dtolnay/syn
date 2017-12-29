use super::*;
use delimited::Delimited;

ast_struct! {
    /// Struct or enum sent to a `proc_macro_derive` macro.
    pub struct DeriveInput {
        /// Attributes tagged on the whole struct or enum.
        pub attrs: Vec<Attribute>,

        /// Visibility of the struct or enum.
        pub vis: Visibility,

        /// Name of the struct or enum.
        pub ident: Ident,

        /// Generics required to complete the definition.
        pub generics: Generics,

        /// Data within the struct or enum.
        pub body: Body,
    }
}

ast_enum_of_structs! {
    /// Body of a derived struct or enum.
    pub enum Body {
        /// It's an enum.
        pub Enum(BodyEnum {
            pub enum_token: Token![enum],
            pub brace_token: token::Brace,
            pub variants: Delimited<Variant, Token![,]>,
        }),

        /// It's a struct.
        pub Struct(BodyStruct {
            pub data: VariantData,
            pub struct_token: Token![struct],
            pub semi_token: Option<Token![;]>,
        }),
    }

    do_not_generate_to_tokens
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::Synom;

    impl Synom for DeriveInput {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::parse_outer) >>
            vis: syn!(Visibility) >>
            which: alt!(
                keyword!(struct) => { Ok }
                |
                keyword!(enum) => { Err }
            ) >>
            id: syn!(Ident) >>
            generics: syn!(Generics) >>
            item: switch!(value!(which),
                Ok(s) => map!(struct_body, move |(wh, body, semi)| DeriveInput {
                    ident: id,
                    vis: vis,
                    attrs: attrs,
                    generics: Generics {
                        where_clause: wh,
                        .. generics
                    },
                    body: Body::Struct(BodyStruct {
                        struct_token: s,
                        data: body,
                        semi_token: semi,
                    }),
                })
                |
                Err(e) => map!(enum_body, move |(wh, body, brace)| DeriveInput {
                    ident: id,
                    vis: vis,
                    attrs: attrs,
                    generics: Generics {
                        where_clause: wh,
                        .. generics
                    },
                    body: Body::Enum(BodyEnum {
                        variants: body,
                        brace_token: brace,
                        enum_token: e,
                    }),
                })
            ) >>
            (item)
        ));

        fn description() -> Option<&'static str> {
            Some("derive input")
        }
    }

    named!(struct_body -> (Option<WhereClause>, VariantData, Option<Token![;]>), alt!(
        do_parse!(
            wh: option!(syn!(WhereClause)) >>
            body: struct_like_body >>
            (wh, VariantData::Struct(body.0, body.1), None)
        )
        |
        do_parse!(
            body: tuple_like_body >>
            wh: option!(syn!(WhereClause)) >>
            semi: punct!(;) >>
            (wh, VariantData::Tuple(body.0, body.1), Some(semi))
        )
        |
        do_parse!(
            wh: option!(syn!(WhereClause)) >>
            semi: punct!(;) >>
            (wh, VariantData::Unit, Some(semi))
        )
    ));

    named!(enum_body -> (Option<WhereClause>, Delimited<Variant, Token![,]>, token::Brace), do_parse!(
        wh: option!(syn!(WhereClause)) >>
        data: braces!(Delimited::parse_terminated) >>
        (wh, data.0, data.1)
    ));

    impl Synom for Variant {
        named!(parse -> Self, do_parse!(
            attrs: many0!(Attribute::parse_outer) >>
            id: syn!(Ident) >>
            data: alt!(
                struct_like_body => { |(d, b)| VariantData::Struct(d, b) }
                |
                tuple_like_body => { |(d, b)| VariantData::Tuple(d, b) }
                |
                epsilon!() => { |_| VariantData::Unit }
            ) >>
            disr: option!(tuple!(punct!(=), syn!(Expr))) >>
            (Variant {
                ident: id,
                attrs: attrs,
                data: data,
                discriminant: disr,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("enum variant")
        }
    }

    named!(struct_like_body -> (Delimited<Field, Token![,]>, token::Brace),
           braces!(call!(Delimited::parse_terminated_with, Field::parse_struct)));

    named!(tuple_like_body -> (Delimited<Field, Token![,]>, token::Paren),
           parens!(call!(Delimited::parse_terminated_with, Field::parse_tuple)));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{ToTokens, Tokens};

    impl ToTokens for DeriveInput {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in self.attrs.outer() {
                attr.to_tokens(tokens);
            }
            self.vis.to_tokens(tokens);
            match self.body {
                Body::Enum(ref d) => d.enum_token.to_tokens(tokens),
                Body::Struct(ref d) => d.struct_token.to_tokens(tokens),
            }
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            match self.body {
                Body::Enum(ref data) => {
                    self.generics.where_clause.to_tokens(tokens);
                    data.brace_token.surround(tokens, |tokens| {
                        data.variants.to_tokens(tokens);
                    });
                }
                Body::Struct(ref data) => {
                    match data.data {
                        VariantData::Struct(..) => {
                            self.generics.where_clause.to_tokens(tokens);
                            data.data.to_tokens(tokens);
                        }
                        VariantData::Tuple(..) => {
                            data.data.to_tokens(tokens);
                            self.generics.where_clause.to_tokens(tokens);
                        }
                        VariantData::Unit => {
                            self.generics.where_clause.to_tokens(tokens);
                        }
                    }
                    data.semi_token.to_tokens(tokens);
                }
            }
        }
    }
}
