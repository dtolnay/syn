use super::*;

use attr::attribute;
use common::{word, visibility};
use generics::generics;
use ty::ty;
use nom::multispace;

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
    Struct(Style, Vec<Field>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variant {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub style: Style,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Style {
    Struct,
    Tuple,
    Unit,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub ident: Option<Ident>,
    pub vis: Visibility,
    pub attrs: Vec<Attribute>,
    pub ty: Ty,
}

named!(pub item<&str, Item>, do_parse!(
    attrs: many0!(attribute) >>
    vis: visibility >>
    which: alt!(tag_s!("struct") | tag_s!("enum")) >>
    multispace >>
    ident: word >>
    generics: generics >>
    item: switch!(value!(which),
        "struct" => map!(struct_body, move |(style, fields)| Item {
            ident: ident,
            vis: vis,
            attrs: attrs,
            generics: generics,
            body: Body::Struct(style, fields),
        })
        |
        "enum" => map!(enum_body, move |body| Item {
            ident: ident,
            vis: vis,
            attrs: attrs,
            generics: generics,
            body: body,
        })
    ) >>
    opt!(multispace) >>
    (item)
));

named!(struct_body<&str, (Style, Vec<Field>)>, alt!(
    struct_like_body => { |fields| (Style::Struct, fields) }
    |
    terminated!(tuple_like_body, punct!(";")) => { |fields| (Style::Tuple, fields) }
    |
    punct!(";") => { |_| (Style::Unit, Vec::new()) }
));

named!(enum_body<&str, Body>, do_parse!(
    punct!("{") >>
    variants: separated_list!(punct!(","), variant) >>
    opt!(punct!(",")) >>
    punct!("}") >>
    (Body::Enum(variants))
));

named!(variant<&str, Variant>, do_parse!(
    attrs: many0!(attribute) >>
    ident: word >>
    body: alt!(
        struct_like_body => { |fields| (Style::Struct, fields) }
        |
        tuple_like_body => { |fields| (Style::Tuple, fields) }
        |
        epsilon!() => { |_| (Style::Unit, Vec::new()) }
    ) >>
    (Variant {
        ident: ident,
        attrs: attrs,
        style: body.0,
        fields: body.1,
    })
));

named!(struct_like_body<&str, Vec<Field> >, do_parse!(
    punct!("{") >>
    fields: separated_list!(punct!(","), struct_field) >>
    opt!(punct!(",")) >>
    punct!("}") >>
    (fields)
));

named!(tuple_like_body<&str, Vec<Field> >, do_parse!(
    punct!("(") >>
    fields: separated_list!(punct!(","), tuple_field) >>
    opt!(punct!(",")) >>
    punct!(")") >>
    (fields)
));

named!(struct_field<&str, Field>, do_parse!(
    attrs: many0!(attribute) >>
    vis: visibility >>
    ident: word >>
    punct!(":") >>
    ty: ty >>
    (Field {
        ident: Some(ident),
        vis: vis,
        attrs: attrs,
        ty: ty,
    })
));

named!(tuple_field<&str, Field>, do_parse!(
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
