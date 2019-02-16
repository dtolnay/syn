use indexmap::IndexMap;
use serde::{Deserialize, Deserializer};

use std::collections::BTreeMap;
use std::ops;

#[derive(Debug, Clone, PartialEq)]
pub struct Definitions {
    pub types: Vec<Node>,
    pub tokens: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Data {
    Private,
    #[serde(rename = "fields")]
    Struct(IndexMap<String, Type>),
    #[serde(rename = "variants")]
    Enum(Vec<Variant>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variant {
    pub ident: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Punctuated {
    pub element: Box<Type>,
    pub punct: String,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct Features {
    any: Vec<String>,
}

impl Variant {
    pub fn new(ident: String, fields: Vec<Type>) -> Variant {
        Variant { ident, fields }
    }
}

impl Punctuated {
    pub fn new(element: Type, punct: String) -> Self {
        Punctuated {
            element: Box::new(element),
            punct,
        }
    }
}

impl Features {
    pub fn new(any: Vec<String>) -> Features {
        Features { any }
    }

    pub fn join(&mut self, other: &Features) {
        if self.any.is_empty() {
            self.any = other.any.clone();
        } else if self.any.len() < other.any.len() {
            assert!(self.any.iter().all(|f| other.any.contains(f)));
        } else {
            assert!(other.any.iter().all(|f| self.any.contains(f)));

            self.any = other.any.clone();
        }
    }

    pub fn len(&self) -> usize {
        self.any.len()
    }

    pub fn contains(&self, tag: &str) -> bool {
        self.iter().any(|s| s == tag)
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.any.iter().map(|s| &s[..])
    }
}

impl ops::Index<usize> for Features {
    type Output = str;

    fn index(&self, index: usize) -> &str {
        self.any.index(index)
    }
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
