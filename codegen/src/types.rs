use std::ops;

#[derive(Debug, Serialize)]
#[serde(tag = "node", rename_all = "lowercase")]
pub enum TypeDef {
    Struct(Struct),
    Enum(Enum),
}

#[derive(Debug, Serialize)]
pub struct Struct {
    ident: String,
    features: Features,
    fields: Vec<Field>,
    all_fields_pub: bool,
}

#[derive(Debug, Serialize)]
pub struct Enum {
    ident: String,
    features: Features,
    variants: Vec<Variant>,
}

#[derive(Debug, Serialize)]
pub struct Variant {
    ident: String,
    fields: Vec<Type>,
}

#[derive(Debug, Serialize)]
pub struct Field {
    ident: String,
    #[serde(rename = "type")]
    ty: Type,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Type {
    /// Type defined by `syn`
    Item(String),

    /// Type defined in `std`.
    Std(String),

    /// Type external to `syn`
    Ext(String),

    /// Token type
    Token(Token),

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
pub struct Token {
    repr: String,
    #[serde(rename = "type")]
    ty: String,
}

#[derive(Debug, Serialize)]
pub struct Punctuated {
    element: Box<Type>,
    punct: Token,
}

#[derive(Debug, Default, Clone, Serialize)]
pub struct Features {
    any: Vec<String>,
}

impl TypeDef {
    pub fn ident(&self) -> &str {
        match self {
            TypeDef::Struct(i) => &i.ident,
            TypeDef::Enum(i) => &i.ident,
        }
    }

    pub fn features(&self) -> &Features {
        match self {
            TypeDef::Struct(i) => &i.features,
            TypeDef::Enum(i) => &i.features,
        }
    }
}

impl Struct {
    pub fn new(
        ident: String,
        features: Features,
        fields: Vec<Field>,
        all_fields_pub: bool,
    ) -> Struct {
        Struct {
            ident,
            features,
            fields,
            all_fields_pub,
        }
    }

    pub fn features(&self) -> &Features {
        &self.features
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn all_fields_pub(&self) -> bool {
        self.all_fields_pub
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

impl Field {
    pub fn new(ident: String, ty: Type) -> Field {
        Field { ident, ty }
    }

    pub fn ident(&self) -> &str {
        &self.ident
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Token {
    pub fn new(repr: String, ty: String) -> Token {
        Token { repr, ty }
    }

    pub fn is_keyword(&self) -> bool {
        let c = self.repr.chars().next().unwrap();
        c.is_alphabetic()
    }

    pub fn repr(&self) -> &str {
        &self.repr
    }
}

impl Punctuated {
    pub fn new(element: Type, punct: Token) -> Self {
        Punctuated { element: Box::new(element), punct }
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
