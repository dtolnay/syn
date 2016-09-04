use super::*;

#[cfg(feature = "parsing")]
use common::word;
#[cfg(feature = "parsing")]
use generics::{lifetime, lifetime_def, ty_param_bound, bound_lifetimes};
#[cfg(feature = "parsing")]
use nom::{digit, multispace};

#[cfg(feature = "parsing")]
use std::str;

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
    pub mutability: Mutability,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
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
pub struct PolyTraitRef {
    /// The `'a` in `<'a> Foo<&'a T>`
    pub bound_lifetimes: Vec<LifetimeDef>,
    /// The `Foo<&'a T>` in `<'a> Foo<&'a T>`
    pub trait_ref: Path,
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

#[cfg(feature = "parsing")]
named!(pub ty<&str, Ty>, alt!(
    ty_vec
    |
    ty_fixed_length_vec
    |
    ty_ptr
    |
    ty_rptr
    |
    ty_bare_fn
    |
    ty_never
    |
    ty_tup
    |
    ty_path
    |
    ty_qpath
    |
    ty_impl_trait
    |
    ty_paren
));

#[cfg(feature = "parsing")]
named!(ty_vec<&str, Ty>, do_parse!(
    punct!("[") >>
    elem: ty >>
    punct!("]") >>
    (Ty::Vec(Box::new(elem)))
));

#[cfg(feature = "parsing")]
named!(ty_fixed_length_vec<&str, Ty>, do_parse!(
    punct!("[") >>
    elem: ty >>
    punct!(";") >>
    opt!(multispace) >>
    size: map_res!(digit, str::parse) >>
    (Ty::FixedLengthVec(Box::new(elem), size))
));

#[cfg(feature = "parsing")]
named!(ty_ptr<&str, Ty>, do_parse!(
    punct!("*") >>
    mutability: alt!(
        punct!("const") => { |_| Mutability::Immutable }
        |
        punct!("mut") => { |_| Mutability::Mutable }
    ) >>
    target: ty >>
    (Ty::Ptr(Box::new(MutTy {
        ty: target,
        mutability: mutability,
    })))
));

#[cfg(feature = "parsing")]
named!(ty_rptr<&str, Ty>, do_parse!(
    punct!("&") >>
    life: opt!(lifetime) >>
    mutability: mutability >>
    target: ty >>
    (Ty::Rptr(life, Box::new(MutTy {
        ty: target,
        mutability: mutability,
    })))
));

#[cfg(feature = "parsing")]
named!(ty_bare_fn<&str, Ty>, do_parse!(
    punct!("fn") >>
    multispace >>
    lifetimes: opt_vec!(delimited!(
        punct!("<"),
        separated_list!(punct!(","), lifetime_def),
        punct!(">")
    )) >>
    punct!("(") >>
    inputs: separated_list!(punct!(","), fn_arg) >>
    punct!(")") >>
    output: opt!(preceded!(
        punct!("->"),
        ty
    )) >>
    (Ty::BareFn(Box::new(BareFnTy {
        lifetimes: lifetimes,
        decl: FnDecl {
            inputs: inputs,
            output: match output {
                Some(ty) => FunctionRetTy::Ty(ty),
                None => FunctionRetTy::Default,
            },
        },
    })))
));

#[cfg(feature = "parsing")]
named!(ty_never<&str, Ty>, map!(punct!("!"), |_| Ty::Never));

#[cfg(feature = "parsing")]
named!(ty_tup<&str, Ty>, do_parse!(
    punct!("(") >>
    elems: separated_list!(punct!(","), ty) >>
    punct!(")") >>
    (Ty::Tup(elems))
));

#[cfg(feature = "parsing")]
named!(ty_path<&str, Ty>, map!(path, |p| Ty::Path(None, p)));

#[cfg(feature = "parsing")]
named!(ty_qpath<&str, Ty>, do_parse!(
    punct!("<") >>
    this: map!(ty, Box::new) >>
    path: opt!(preceded!(
        tuple!(punct!("as"), multispace),
        path
    )) >>
    punct!(">") >>
    punct!("::") >>
    rest: separated_nonempty_list!(punct!("::"), path_segment) >>
    ({
        match path {
            Some(mut path) => {
                let pos = path.segments.len();
                path.segments.extend(rest);
                Ty::Path(Some(QSelf { ty: this, position: pos }), path)
            }
            None => {
                Ty::Path(Some(QSelf { ty: this, position: 0 }), Path {
                    global: false,
                    segments: rest,
                })
            }
        }
    })
));

#[cfg(feature = "parsing")]
named!(ty_impl_trait<&str, Ty>, do_parse!(
    punct!("impl") >>
    multispace >>
    elem: separated_nonempty_list!(punct!("+"), ty_param_bound) >>
    (Ty::ImplTrait(elem))
));

#[cfg(feature = "parsing")]
named!(ty_paren<&str, Ty>, do_parse!(
    punct!("(") >>
    elem: ty >>
    punct!(")") >>
    (Ty::Paren(Box::new(elem)))
));

#[cfg(feature = "parsing")]
named!(mutability<&str, Mutability>, preceded!(
    opt!(multispace),
    alt!(
        terminated!(tag_s!("mut"), multispace) => { |_| Mutability::Mutable }
        |
        epsilon!() => { |_| Mutability::Immutable }
    )
));

#[cfg(feature = "parsing")]
named!(path<&str, Path>, do_parse!(
    global: opt!(punct!("::")) >>
    segments: separated_nonempty_list!(punct!("::"), path_segment) >>
    (Path {
        global: global.is_some(),
        segments: segments,
    })
));

#[cfg(feature = "parsing")]
named!(path_segment<&str, PathSegment>, alt!(
    do_parse!(
        ident: word >>
        punct!("<") >>
        lifetimes: separated_list!(punct!(","), lifetime) >>
        types: opt_vec!(preceded!(
            cond!(!lifetimes.is_empty(), punct!(",")),
            separated_nonempty_list!(
                punct!(","),
                terminated!(ty, not!(peek!(punct!("="))))
            )
        )) >>
        bindings: opt_vec!(preceded!(
            cond!(!lifetimes.is_empty() || !types.is_empty(), punct!(",")),
            separated_nonempty_list!(punct!(","), type_binding)
        )) >>
        punct!(">") >>
        (PathSegment {
            ident: ident,
            parameters: PathParameters::AngleBracketed(
                AngleBracketedParameterData {
                    lifetimes: lifetimes,
                    types: types,
                    bindings: bindings,
                }
            ),
        })
    )
    |
    map!(word, PathSegment::ident)
));

#[cfg(feature = "parsing")]
named!(type_binding<&str, TypeBinding>, do_parse!(
    ident: word >>
    punct!("=") >>
    ty: ty >>
    (TypeBinding {
        ident: ident,
        ty: ty,
    })
));

#[cfg(feature = "parsing")]
named!(pub poly_trait_ref<&str, PolyTraitRef>, do_parse!(
    bound_lifetimes: bound_lifetimes >>
    trait_ref: path >>
    (PolyTraitRef {
        bound_lifetimes: bound_lifetimes,
        trait_ref: trait_ref,
    })
));

#[cfg(feature = "parsing")]
named!(fn_arg<&str, Arg>, do_parse!(
    pat: opt!(terminated!(word, punct!(":"))) >>
    ty: ty >>
    (Arg {
        pat: pat,
        ty: ty,
    })
));
