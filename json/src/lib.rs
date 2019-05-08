//! # Data structures that describe Syn's syntax tree.
//!
//! The Syn syntax tree is made up of more than 200 types. Occasionally it can
//! come up that you need to implement some behavior across them all.
//!
//! - For example [the Rust integration for AST Explorer][astexplorer] wants to
//!   turn a syntax tree from Syn into a JavaScript value understood by the
//!   platform's existing cross-language syntax tree visualization code.
//!
//!   [astexplorer]: https://astexplorer.net/#/gist/388150a52f74d45a355d2b5e865ded96/0c6d563f28d900472f699c21a1845ad20ae9927f
//!
//! - As another example from within Syn itself, the traits and implementations
//!   of the [`visit`], [`visit_mut`], and [`fold`] modules can be generated
//!   programmatically from a description of the syntax tree.
//!
//!   [`visit`]: https://docs.rs/syn/0.15/syn/visit/index.html
//!   [`visit_mut`]: https://docs.rs/syn/0.15/syn/visit_mut/index.html
//!   [`fold`]: https://docs.rs/syn/0.15/syn/fold/index.html
//!
//! To make this type of code as easy as possible to implement in any language,
//! every Syn release comes with a machine-readable description of that version
//! of the syntax tree as a JSON file [syn.json]. This `syn-codegen` crate
//! provides the canonical data structures for parsing and making use of the
//! representation in syn.json from Rust code.
//!
//! [syn.json]: https://raw.githubusercontent.com/dtolnay/syn/master/syn.json

use indexmap::IndexMap;
use semver::Version;
use serde::{Deserialize, Deserializer, Serialize};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Definitions {
    /// The Syn version used to generate the introspection file.
    pub version: Version,
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
        deserialize_with = "private_if_absent"
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

fn private_if_absent<'de, D>(deserializer: D) -> Result<Data, D::Error>
where
    D: Deserializer<'de>,
{
    let option = Option::deserialize(deserializer)?;
    Ok(option.unwrap_or(Data::Private))
}
