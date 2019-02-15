use indexmap::IndexMap;

use std::collections::BTreeMap;
use std::ops;

#[derive(Debug, Clone)]
pub struct Definitions {
    pub types: Vec<Node>,
    pub tokens: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "node", rename_all = "lowercase")]
pub enum Node {
    Struct(Struct),
    Enum(Enum),
}

#[derive(Debug, Clone, Serialize)]
pub struct Struct {
    ident: String,
    features: Features,
    #[serde(skip_serializing_if = "IndexMap::is_empty")]
    fields: IndexMap<String, Type>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Enum {
    ident: String,
    features: Features,
    variants: Vec<Variant>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Variant {
    ident: String,
    fields: Vec<Type>,
}

#[derive(Debug, Clone, Serialize)]
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

#[derive(Debug, Clone, Serialize)]
pub struct Punctuated {
    element: Box<Type>,
    punct: String,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct Features {
    any: Vec<String>,
}

impl Definitions {
    pub fn insert(&mut self, node: Node) {
        self.types.push(node);
    }
}

impl Node {
    pub fn ident(&self) -> &str {
        match self {
            Node::Struct(i) => &i.ident,
            Node::Enum(i) => &i.ident,
        }
    }

    pub fn features(&self) -> &Features {
        match self {
            Node::Struct(i) => &i.features,
            Node::Enum(i) => &i.features,
        }
    }
}

impl Struct {
    pub fn new(ident: String, features: Features, fields: IndexMap<String, Type>) -> Struct {
        Struct {
            ident,
            features,
            fields,
        }
    }

    pub fn features(&self) -> &Features {
        &self.features
    }

    pub fn fields(&self) -> &IndexMap<String, Type> {
        &self.fields
    }
}

impl Enum {
    pub fn new(ident: String, features: Features, variants: Vec<Variant>) -> Enum {
        Enum {
            ident,
            features,
            variants,
        }
    }

    pub fn variants(&self) -> &[Variant] {
        &self.variants
    }
}

impl Variant {
    pub fn new(ident: String, fields: Vec<Type>) -> Variant {
        Variant { ident, fields }
    }

    pub fn ident(&self) -> &str {
        &self.ident
    }

    pub fn fields(&self) -> &[Type] {
        &self.fields
    }
}

impl Punctuated {
    pub fn new(element: Type, punct: String) -> Self {
        Punctuated {
            element: Box::new(element),
            punct,
        }
    }

    pub fn element(&self) -> &Type {
        &self.element
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
