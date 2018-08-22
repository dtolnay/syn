use proc_macro2::Ident;

use parse::{Parse, ParseStream, Result};
use token;

/// Things that can appear directly inside of a module or scope.
#[derive(Debug)]
pub enum Item {
    Struct(ItemStruct),
    Enum(ItemEnum),
}

/// A struct definition: `struct S { a: A, b: B }`.
#[derive(Debug)]
pub struct ItemStruct {
    pub struct_token: Token![struct],
    pub ident: Ident,
    pub brace_token: token::Brace,
    pub fields: Vec<Field>,
}

/// An enum definition: `enum E { A, B, C }`.
#[derive(Debug)]
pub struct ItemEnum {
    pub enum_token: Token![enum],
    pub ident: Ident,
    pub brace_token: token::Brace,
    pub variants: Vec<Variant>,
}

/// A named field of a braced struct.
#[derive(Debug)]
pub struct Field {
    pub name: Ident,
    pub colon_token: Token![:],
    pub ty: Ident,
    pub comma_token: Token![,],
}

/// An enum variant.
#[derive(Debug)]
pub struct Variant {
    pub name: Ident,
    pub comma_token: token::Comma,
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![struct]) {
            input.parse().map(Item::Struct)
        } else if lookahead.peek(Token![enum]) {
            input.parse().map(Item::Enum)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for ItemStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(ItemStruct {
            struct_token: input.parse()?,
            ident: input.parse()?,
            brace_token: braced!(content in input),
            fields: content.parse()?,
        })
    }
}

impl Parse for ItemEnum {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(ItemEnum {
            enum_token: input.parse()?,
            ident: input.parse()?,
            brace_token: braced!(content in input),
            variants: content.parse()?,
        })
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Field {
            name: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            comma_token: input.parse()?,
        })
    }
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Variant {
            name: input.parse()?,
            comma_token: input.parse()?,
        })
    }
}
