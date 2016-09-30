use super::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variant {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub data: VariantData,
    /// Explicit discriminant, e.g. `Foo = 1`
    pub discriminant: Option<Discriminant>,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Discriminant {
    pub value: u64,
    pub ty: IntTy,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use attr::parsing::attribute;
    use ident::parsing::ident;
    use lit::parsing::int;
    use ty::parsing::ty;

    named!(pub struct_body -> VariantData, alt!(
        struct_like_body => { VariantData::Struct }
        |
        terminated!(tuple_like_body, punct!(";")) => { VariantData::Tuple }
        |
        punct!(";") => { |_| VariantData::Unit }
    ));

    named!(pub enum_body -> Vec<Variant>, do_parse!(
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
        disr: option!(preceded!(punct!("="), discriminant)) >>
        (Variant {
            ident: id,
            attrs: attrs,
            data: data,
            discriminant: disr,
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
            keyword!("pub") >>
            (Visibility::Public)
        )
        |
        epsilon!() => { |_| Visibility::Inherited }
    ));

    named!(discriminant -> Discriminant, map!(
        int,
        |(value, ty)| Discriminant {
            value: value,
            ty: ty,
        }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use lit::Lit;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Variant {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in &self.attrs {
                attr.to_tokens(tokens);
            }
            self.ident.to_tokens(tokens);
            self.data.to_tokens(tokens);
            if let Some(ref disr) = self.discriminant {
                tokens.append("=");
                disr.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for VariantData {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
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

    impl ToTokens for Discriminant {
        fn to_tokens(&self, tokens: &mut Tokens) {
            Lit::Int(self.value, self.ty).to_tokens(tokens);
        }
    }
}
