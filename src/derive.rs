use super::*;
use delimited::Delimited;

ast_struct! {
    /// Struct or enum sent to a `proc_macro_derive` macro.
    pub struct DeriveInput {
        /// Name of the struct or enum.
        pub ident: Ident,

        /// Visibility of the struct or enum.
        pub vis: Visibility,

        /// Attributes tagged on the whole struct or enum.
        pub attrs: Vec<Attribute>,

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
            pub enum_token: tokens::Enum,
            pub brace_token: tokens::Brace,
            pub variants: Delimited<Variant, tokens::Comma>,
        }),

        /// It's a struct.
        pub Struct(BodyStruct {
            pub data: VariantData,
            pub struct_token: tokens::Struct,
            pub semi_token: Option<tokens::Semi>,
        }),
    }

    do_not_generate_to_tokens
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::Synom;
    use synom::tokens::*;

    impl Synom for DeriveInput {
        named!(parse -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_outer)) >>
            vis: syn!(Visibility) >>
            which: alt!(
                syn!(Struct) => { Ok }
                |
                // weird hack to get around exhaustiveness check below
                syn!(Enum) => { |e| Err((e, 1)) }
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
                Err((e, 1)) => map!(enum_body, move |(wh, body, brace)| DeriveInput {
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


    named!(struct_body -> (WhereClause, VariantData, Option<tokens::Semi>), alt!(
        do_parse!(
            wh: syn!(WhereClause) >>
            body: struct_like_body >>
            (wh, VariantData::Struct(body.0, body.1), None)
        )
        |
        do_parse!(
            body: tuple_like_body >>
            wh: syn!(WhereClause) >>
            semi: syn!(Semi) >>
            (wh, VariantData::Tuple(body.0, body.1), Some(semi))
        )
        |
        do_parse!(
            wh: syn!(WhereClause) >>
            semi: syn!(Semi) >>
            (wh, VariantData::Unit, Some(semi))
        )
    ));

    named!(enum_body -> (WhereClause, Delimited<Variant, tokens::Comma>, tokens::Brace), do_parse!(
        wh: syn!(WhereClause) >>
        data: braces!(Delimited::parse_terminated) >>
        (wh, data.0, data.1)
    ));

    impl Synom for Variant {
        named!(parse -> Self, do_parse!(
            attrs: many0!(call!(Attribute::parse_outer)) >>
            id: syn!(Ident) >>
            data: alt!(
                struct_like_body => { |(d, b)| VariantData::Struct(d, b) }
                |
                tuple_like_body => { |(d, b)| VariantData::Tuple(d, b) }
                |
                epsilon!() => { |_| VariantData::Unit }
            ) >>
            disr: option!(do_parse!(
                eq: syn!(Eq) >>
                disr: syn!(Expr) >>
                (eq, disr)
            )) >>
            (Variant {
                ident: id,
                attrs: attrs,
                data: data,
                eq_token: disr.as_ref().map(|p| tokens::Eq((p.0).0)),
                discriminant: disr.map(|p| p.1),
            })
        ));
    }

    named!(struct_like_body -> (Delimited<Field, tokens::Comma>, tokens::Brace),
           braces!(call!(Delimited::parse_terminated_with, Field::parse_struct)));

    named!(tuple_like_body -> (Delimited<Field, tokens::Comma>, tokens::Paren),
           parens!(call!(Delimited::parse_terminated_with, Field::parse_tuple)));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{Tokens, ToTokens};

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
