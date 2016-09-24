use super::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MacroInput {
    pub ident: Ident,
    pub vis: Visibility,
    pub attrs: Vec<Attribute>,
    pub generics: Generics,
    pub body: Body,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Body {
    Enum(Vec<Variant>),
    Struct(VariantData),
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use attr::parsing::attribute;
    use data::parsing::{visibility, struct_body, enum_body};
    use generics::parsing::generics;
    use ident::parsing::ident;
    use nom::multispace;

    named!(pub macro_input -> MacroInput, do_parse!(
        attrs: many0!(attribute) >>
        vis: visibility >>
        which: alt!(punct!("struct") | punct!("enum")) >>
        multispace >>
        id: ident >>
        generics: generics >>
        item: switch!(value!(which),
            "struct" => map!(struct_body, move |body| MacroInput {
                ident: id,
                vis: vis,
                attrs: attrs,
                generics: generics,
                body: Body::Struct(body),
            })
            |
            "enum" => map!(enum_body, move |body| MacroInput {
                ident: id,
                vis: vis,
                attrs: attrs,
                generics: generics,
                body: Body::Enum(body),
            })
        ) >>
        option!(multispace) >>
        (item)
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use data::{Visibility, VariantData};
    use quote::{Tokens, ToTokens};

    impl ToTokens for MacroInput {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in &self.attrs {
                attr.to_tokens(tokens);
            }
            if let Visibility::Public = self.vis {
                tokens.append("pub");
            }
            match self.body {
                Body::Enum(_) => tokens.append("enum"),
                Body::Struct(_) => tokens.append("struct"),
            }
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for Body {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Body::Enum(ref variants) => {
                    tokens.append("{");
                    for variant in variants {
                        variant.to_tokens(tokens);
                        tokens.append(",");
                    }
                    tokens.append("}");
                }
                Body::Struct(ref variant_data) => {
                    variant_data.to_tokens(tokens);
                    match *variant_data {
                        VariantData::Struct(_) => { /* no semicolon */ }
                        VariantData::Tuple(_) |
                        VariantData::Unit => tokens.append(";"),
                    }
                }
            }
        }
    }
}
