use super::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Item {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variant {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub data: VariantData,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VariantData {
    Struct(Vec<Field>),
    Tuple(Vec<Field>),
    Unit,
}

impl VariantData {
    pub fn fields(&self) -> &[Field] {
        match *self {
            VariantData::Struct(ref fields) |
            VariantData::Tuple(ref fields) => fields,
            VariantData::Unit => &[],
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub ident: Option<Ident>,
    pub vis: Visibility,
    pub attrs: Vec<Attribute>,
    pub ty: Ty,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Inherited,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use attr::parsing::attribute;
    use generics::parsing::generics;
    use ident::parsing::ident;
    use ty::parsing::ty;
    use nom::multispace;

    named!(pub item -> Item, do_parse!(
        attrs: many0!(attribute) >>
        vis: visibility >>
        which: alt!(punct!("struct") | punct!("enum")) >>
        multispace >>
        id: ident >>
        generics: generics >>
        item: switch!(value!(which),
            "struct" => map!(struct_body, move |body| Item {
                ident: id,
                vis: vis,
                attrs: attrs,
                generics: generics,
                body: Body::Struct(body),
            })
            |
            "enum" => map!(enum_body, move |body| Item {
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

    named!(struct_body -> VariantData, alt!(
        struct_like_body => { VariantData::Struct }
        |
        terminated!(tuple_like_body, punct!(";")) => { VariantData::Tuple }
        |
        punct!(";") => { |_| VariantData::Unit }
    ));

    named!(enum_body -> Vec<Variant>, do_parse!(
        punct!("{") >>
        variants: separated_list!(punct!(","), variant) >>
        option!(punct!(",")) >>
        punct!("}") >>
        (variants)
    ));

    named!(variant -> Variant, do_parse!(
        attrs: many0!(attribute) >>
        id: ident >>
        data: alt!(
            struct_like_body => { VariantData::Struct }
            |
            tuple_like_body => { VariantData::Tuple }
            |
            epsilon!() => { |_| VariantData::Unit }
        ) >>
        (Variant {
            ident: id,
            attrs: attrs,
            data: data,
        })
    ));

    named!(struct_like_body -> Vec<Field>, do_parse!(
        punct!("{") >>
        fields: separated_list!(punct!(","), struct_field) >>
        option!(punct!(",")) >>
        punct!("}") >>
        (fields)
    ));

    named!(tuple_like_body -> Vec<Field>, do_parse!(
        punct!("(") >>
        fields: separated_list!(punct!(","), tuple_field) >>
        option!(punct!(",")) >>
        punct!(")") >>
        (fields)
    ));

    named!(struct_field -> Field, do_parse!(
        attrs: many0!(attribute) >>
        vis: visibility >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        (Field {
            ident: Some(id),
            vis: vis,
            attrs: attrs,
            ty: ty,
        })
    ));

    named!(tuple_field -> Field, do_parse!(
        attrs: many0!(attribute) >>
        vis: visibility >>
        ty: ty >>
        (Field {
            ident: None,
            vis: vis,
            attrs: attrs,
            ty: ty,
        })
    ));

    named!(pub visibility -> Visibility, alt!(
        do_parse!(
            punct!("pub") >>
            multispace >>
            (Visibility::Public)
        )
        |
        epsilon!() => { |_| Visibility::Inherited }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Item {
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
                    variant_data_to_tokens(variant_data, tokens);
                    match *variant_data {
                        VariantData::Struct(_) => { /* no semicolon */ }
                        VariantData::Tuple(_) |
                        VariantData::Unit => tokens.append(";"),
                    }
                }
            }
        }
    }

    impl ToTokens for Variant {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in &self.attrs {
                attr.to_tokens(tokens);
            }
            self.ident.to_tokens(tokens);
            variant_data_to_tokens(&self.data, tokens);
        }
    }

    impl ToTokens for Field {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in &self.attrs {
                attr.to_tokens(tokens);
            }
            if let Visibility::Public = self.vis {
                tokens.append("pub");
            }
            if let Some(ref ident) = self.ident {
                ident.to_tokens(tokens);
                tokens.append(":");
            }
            self.ty.to_tokens(tokens);
        }
    }

    fn variant_data_to_tokens(data: &VariantData, tokens: &mut Tokens) {
        match *data {
            VariantData::Struct(ref fields) => {
                tokens.append("{");
                tokens.append_separated(fields, ",");
                tokens.append("}");
            }
            VariantData::Tuple(ref fields) => {
                tokens.append("(");
                tokens.append_separated(fields, ",");
                tokens.append(")");
            }
            VariantData::Unit => {}
        }
    }
}
