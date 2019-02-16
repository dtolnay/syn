use indexmap::IndexMap;
use serde::{Deserialize, Deserializer, Serialize};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Definitions {
    /// The Syn version used to generate the introspection file.
    pub version: String,
    pub types: Vec<Node>,
    pub tokens: BTreeMap<String, String>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Node {
    pub ident: String,
    pub features: Features,
    #[serde(
        flatten,
        skip_serializing_if = "is_private",
        deserialize_with = "de_data"
    )]
    pub data: Data,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Data {
    Private,
    #[serde(rename = "fields")]
    Struct(Fields),
    #[serde(rename = "variants")]
    Enum(Variants),
}

pub type Fields = IndexMap<String, Type>;
pub type Variants = IndexMap<String, Vec<Type>>;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Type {
    /// Type defined by `syn`
    Syn(String),

    /// Type defined in `std`.
    Std(String),

    /// Type external to `syn`
    #[serde(rename = "proc_macro2")]
    Ext(String),

    /// Token type
    Token(String),

    /// Token group
    Group(String),

    /// Punctuated list
    Punctuated(Punctuated),

    Option(Box<Type>),
    Box(Box<Type>),
    Vec(Box<Type>),
    Tuple(Vec<Type>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Punctuated {
    pub element: Box<Type>,
    pub punct: String,
}

#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Features {
    pub any: BTreeSet<String>,
}

fn is_private(data: &Data) -> bool {
    match data {
        Data::Private => true,
        Data::Struct(_) | Data::Enum(_) => false,
    }
}

fn de_data<'de, D>(deserializer: D) -> Result<Data, D::Error>
where
    D: Deserializer<'de>,
{
    let option = Option::deserialize(deserializer)?;
    Ok(option.unwrap_or(Data::Private))
}
