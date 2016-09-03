#[macro_use]
extern crate nom;

use nom::IResult;
use nom::multispace as space;

use std::str;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Item {
    pub ident: Ident,
    pub vis: Visibility,
    pub attrs: Vec<Attribute>,
    pub generics: Generics,
    pub body: Body,
}

pub type Ident = String;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attribute {
    pub value: MetaItem,
    pub is_sugared_doc: bool,
}

/// A compile-time attribute item.
///
/// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MetaItem {
    /// Word meta item.
    ///
    /// E.g. `test` as in `#[test]`
    Word(Ident),
    /// List meta item.
    ///
    /// E.g. `derive(..)` as in `#[derive(..)]`
    List(Ident, Vec<MetaItem>),
    /// Name value meta item.
    ///
    /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
    NameValue(Ident, String),
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Generics {
    pub lifetimes: Vec<LifetimeDef>,
    pub ty_params: Vec<TyParam>,
    pub where_clause: Vec<WherePredicate>,
}

/// A single predicate in a `where` clause
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WherePredicate {
    /// A type binding, e.g. `for<'c> Foo: Send+Clone+'c`
    BoundPredicate(WhereBoundPredicate),
    /// A lifetime predicate, e.g. `'a: 'b+'c`
    RegionPredicate(WhereRegionPredicate),
}

/// A type bound.
///
/// E.g. `for<'c> Foo: Send+Clone+'c`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhereBoundPredicate {
    /// Any lifetimes from a `for` binding
    pub bound_lifetimes: Vec<LifetimeDef>,
    /// The type being bounded
    pub bounded_ty: Ty,
    /// Trait and lifetime bounds (`Clone+Send+'static`)
    pub bounds: Vec<TyParamBound>,
}

/// A lifetime predicate.
///
/// E.g. `'a: 'b+'c`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhereRegionPredicate {
    pub lifetime: Lifetime,
    pub bounds: Vec<Lifetime>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LifetimeDef {
    pub lifetime: Lifetime,
    pub bounds: Vec<Lifetime>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lifetime {
    pub ident: Ident,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TyParam {
    pub ident: Ident,
    pub bounds: Vec<TyParamBound>,
    pub default: Option<Ty>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TyParamBound {
    MaybeSized,
    Region(Lifetime),
    Trait(PolyTraitRef),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PolyTraitRef {
    /// The `'a` in `<'a> Foo<&'a T>`
    pub bound_lifetimes: Vec<LifetimeDef>,
    /// The `Foo<&'a T>` in `<'a> Foo<&'a T>`
    pub trait_ref: Path,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Path {
    pub global: bool,
    pub segments: Vec<PathSegment>,
}

/// A segment of a path: an identifier, an optional lifetime, and a set of types.
///
/// E.g. `std`, `String` or `Box<T>`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathSegment {
    pub ident: Ident,
    pub parameters: PathParameters,
}

impl PathSegment {
    pub fn ident(ident: Ident) -> Self {
        PathSegment {
            ident: ident,
            parameters: PathParameters::none(),
        }
    }
}

/// Parameters of a path segment.
///
/// E.g. `<A, B>` as in `Foo<A, B>` or `(A, B)` as in `Foo(A, B)`
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PathParameters {
    /// The `<'a, A, B, C>` in `foo::bar::baz::<'a, A, B, C>`
    AngleBracketed(AngleBracketedParameterData),
    /// The `(A, B)` and `C` in `Foo(A, B) -> C`
    Parenthesized(ParenthesizedParameterData),
}

impl PathParameters {
    pub fn none() -> Self {
        PathParameters::AngleBracketed(AngleBracketedParameterData::default())
    }
}

/// A path like `Foo<'a, T>`
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct AngleBracketedParameterData {
    /// The lifetime parameters for this path segment.
    pub lifetimes: Vec<Lifetime>,
    /// The type parameters for this path segment, if present.
    pub types: Vec<Ty>,
    /// Bindings (equality constraints) on associated types, if present.
    ///
    /// E.g., `Foo<A=Bar>`.
    pub bindings: Vec<TypeBinding>,
}

/// Bind a type to an associated type: `A=Foo`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeBinding {
    pub ident: Ident,
    pub ty: Ty,
}

/// A path like `Foo(A,B) -> C`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParenthesizedParameterData {
    /// `(A, B)`
    pub inputs: Vec<Ty>,
    /// `C`
    pub output: Option<Ty>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Visibility {
    Public,
    Inherited,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    /// A variable-length array (`[T]`)
    Vec(Box<Ty>),
    /// A fixed length array (`[T; n]`)
    FixedLengthVec(Box<Ty>, usize),
    /// A raw pointer (`*const T` or `*mut T`)
    Ptr(Box<MutTy>),
    /// A reference (`&'a T` or `&'a mut T`)
    Rptr(Option<Lifetime>, Box<MutTy>),
    /// A bare function (e.g. `fn(usize) -> bool`)
    BareFn(Box<BareFnTy>),
    /// The never type (`!`)
    Never,
    /// A tuple (`(A, B, C, D, ...)`)
    Tup(Vec<Ty>),
    /// A path (`module::module::...::Type`), optionally
    /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
    ///
    /// Type parameters are stored in the Path itself
    Path(Option<QSelf>, Path),
    /// Something like `A+B`. Note that `B` must always be a path.
    ObjectSum(Box<Ty>, Vec<TyParamBound>),
    /// A type like `for<'a> Foo<&'a Bar>`
    PolyTraitRef(Vec<TyParamBound>),
    /// An `impl TraitA+TraitB` type.
    ImplTrait(Vec<TyParamBound>),
    /// No-op; kept solely so that we can pretty-print faithfully
    Paren(Box<Ty>),
    /// TyKind::Infer means the type should be inferred instead of it having been
    /// specified. This can appear anywhere in a type.
    Infer,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MutTy {
    pub ty: Ty,
    pub mutbl: Mutability,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

/// The explicit Self type in a "qualified path". The actual
/// path, including the trait and the associated item, is stored
/// separately. `position` represents the index of the associated
/// item qualified with this Self type.
///
/// ```rust,ignore
/// <Vec<T> as a::b::Trait>::AssociatedItem
///  ^~~~~     ~~~~~~~~~~~~~~^
///  ty        position = 3
///
/// <Vec<T>>::AssociatedItem
///  ^~~~~    ^
///  ty       position = 0
/// ```
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct QSelf {
    pub ty: Box<Ty>,
    pub position: usize
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BareFnTy {
    pub lifetimes: Vec<LifetimeDef>,
    pub decl: FnDecl
}

/// Header (not the body) of a function declaration.
///
/// E.g. `fn foo(bar: baz)`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnDecl {
    pub inputs: Vec<Arg>,
    pub output: FunctionRetTy,
}

/// An argument in a function header.
///
/// E.g. `bar: usize` as in `fn foo(bar: usize)`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Arg {
    pub pat: Option<Ident>,
    pub ty: Ty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctionRetTy {
    /// Return type is not specified.
    ///
    /// Functions default to `()` and
    /// closures default to inference. Span points to where return
    /// type would be inserted.
    Default,
    /// Everything else
    Ty(Ty),
}

named!(attribute<&str, Attribute>, chain!(
    space? ~
    tag_s!("#[") ~
    space? ~
    first: word ~
    space? ~
    tag_s!("(") ~
    inner: separated_list!(tag_s!(", "), word) ~
    tag_s!(")") ~
    space? ~
    tag_s!("]") ,
    || { return  Attribute {
            value: MetaItem::List(first.to_string(), inner.into_iter().map(|w|
                MetaItem::Word(w.to_string())
            ).collect()),
            is_sugared_doc: false,
        } }
));

named!(visibility<&str, Visibility>,
    map!(opt!(terminated!(tag_s!("pub"), space)), |tag| match tag {
        Some(_) => Visibility::Public,
        None => Visibility::Inherited,
    })
);

fn ident_ch(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

named!(word<&str, Ident>, map!(take_while1_s!(ident_ch), String::from));

named!(path_segment<&str, PathSegment>, alt!(
    chain!(
        ident: word ~
        space? ~
        tag_s!("<") ~
        types: many0!(ty) ~
        space? ~
        tag_s!(">") ,
        || {
            PathSegment {
                ident: ident,
                parameters: PathParameters::AngleBracketed(
                    AngleBracketedParameterData {
                        lifetimes: Vec::new(),
                        types: types,
                        bindings: Vec::new(),
                    }
                ),
            }
        }
    )
    |
    map!(word, |n| PathSegment::ident(n))
));

named!(ty<&str, Ty>, chain!(
    global: tag_s!("::")? ~
    segments: separated_nonempty_list!(tag_s!("::"), path_segment) ,
    || {
        Ty::Path(None, Path {
            global: global.is_some(),
            segments: segments,
        })
    }
));

/*
    /// A variable-length array (`[T]`)
    Vec(Box<Ty>),
    /// A fixed length array (`[T; n]`)
    FixedLengthVec(Box<Ty>, usize),
    /// A raw pointer (`*const T` or `*mut T`)
    Ptr(Box<MutTy>),
    /// A reference (`&'a T` or `&'a mut T`)
    Rptr(Option<Lifetime>, Box<MutTy>),
    /// A bare function (e.g. `fn(usize) -> bool`)
    BareFn(Box<BareFnTy>),
    /// The never type (`!`)
    Never,
    /// A tuple (`(A, B, C, D, ...)`)
    Tup(Vec<Ty>),
    /// A path (`module::module::...::Type`), optionally
    /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
    ///
    /// Type parameters are stored in the Path itself
    Path(Option<QSelf>, Path),
    /// Something like `A+B`. Note that `B` must always be a path.
    ObjectSum(Box<Ty>, Vec<TyParamBound>),
    /// A type like `for<'a> Foo<&'a Bar>`
    PolyTraitRef(Vec<TyParamBound>),
    /// An `impl TraitA+TraitB` type.
    ImplTrait(Vec<TyParamBound>),
    /// No-op; kept solely so that we can pretty-print faithfully
    Paren(Box<Ty>),
    /// TyKind::Infer means the type should be inferred instead of it having been
    /// specified. This can appear anywhere in a type.
    Infer,
*/

named!(field<&str, Field>, chain!(
    space? ~
    vis: visibility ~
    ident: word ~
    space? ~
    tag_s!(":") ~
    space? ~
    ty: ty ,
    || {
        Field {
            ident: Some(ident),
            vis: vis,
            attrs: Vec::new(),
            ty: ty,
        }
    }
));

named!(struct_body<&str, Body>, alt!(
    chain!(
        tag_s!("{") ~
        fields: separated_list!(tag_s!(","), field) ~
        space? ~
        tag_s!(",")? ~
        space? ~
        tag_s!("}") ,
        || { Body::Struct(Style::Struct, fields) }
    )
    |
    chain!(
        tag_s!("(") ~
        separated_list!(tag_s!(","), chain!(
            space? ~
            vis: visibility ~
            ty: ty ,
            || {}
        )) ~
        space? ~
        tag_s!(",")? ~
        space? ~
        tag_s!(")") ,
        || { Body::Struct(Style::Tuple, Vec::new()) }
    )
    |
    map!(tag_s!(";"), |_| Body::Struct(Style::Unit, Vec::new()))
));

named!(item<&str, Item>, chain!(
    attrs: many0!(attribute) ~
    space? ~
    vis: visibility ~
    tag_s!("struct") ~
    space ~
    ident: word ~
    space? ~
    body: struct_body ~
    space? ,
    || { return Item {
            ident: ident,
            vis: vis,
            attrs: attrs,
            generics: Generics::default(),
            body: body,
        } }
));

pub fn parse(input: &str) -> Result<Item, String> {
    match item(input) {
        IResult::Done(rest, ast) => {
            if rest.is_empty() {
                Ok(ast)
            } else {
                Err(format!("more than a single input item: {:?}", rest))
            }
        }
        IResult::Error(nom::Err::Code(kind)) |
        IResult::Error(nom::Err::Node(kind, _)) => {
            Err(format!("failed to parse {:?}", kind))
        }
        IResult::Error(nom::Err::Position(kind, pos)) |
        IResult::Error(nom::Err::NodePosition(kind, pos, _)) => {
            Err(format!("failed to parse {:?}: {:?}", kind, pos))
        }
        IResult::Incomplete(_) => Err("incomplete input item".to_string()),
    }
}
