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
    use Generics;
    use attr::parsing::outer_attr;
    use data::parsing::{visibility, struct_body, enum_body};
    use generics::parsing::generics;
    use ident::parsing::ident;

    named!(pub derive_input -> DeriveInput, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        which: alt!(keyword!("struct") | keyword!("enum")) >>
        id: ident >>
        generics: generics >>
        item: switch!(value!(which),
            "struct" => map!(struct_body, move |(wh, body, semi)| DeriveInput {
                ident: id,
                vis: vis,
                attrs: attrs,
                generics: Generics {
                    where_clause: wh,
                    .. generics
                },
                body: Body::Struct(BodyStruct {
                    struct_token: tokens::Struct::default(),
                    data: body,
                    semi_token: semi,
                }),
            })
            |
            "enum" => map!(enum_body, move |(wh, body, brace)| DeriveInput {
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
                    enum_token: tokens::Enum::default(),
                }),
            })
        ) >>
        (item)
    ));
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
