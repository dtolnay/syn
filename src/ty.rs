use delimited::Delimited;
use super::*;

ast_enum_of_structs! {
    /// The different kinds of types recognized by the compiler
    pub enum Ty {
        /// A variable-length array (`[T]`)
        pub Slice(TySlice {
            pub ty: Box<Ty>,
            pub bracket_token: tokens::Bracket,
        }),
        /// A fixed length array (`[T; n]`)
        pub Array(TyArray {
            pub bracket_token: tokens::Bracket,
            pub ty: Box<Ty>,
            pub semi_token: tokens::Semi,
            pub amt: ConstExpr,
        }),
        /// A raw pointer (`*const T` or `*mut T`)
        pub Ptr(TyPtr {
            pub star_token: tokens::Star,
            pub const_token: Option<tokens::Const>,
            pub ty: Box<MutTy>,
        }),
        /// A reference (`&'a T` or `&'a mut T`)
        pub Rptr(TyRptr {
            pub and_token: tokens::And,
            pub lifetime: Option<Lifetime>,
            pub ty: Box<MutTy>,
        }),
        /// A bare function (e.g. `fn(usize) -> bool`)
        pub BareFn(TyBareFn {
            pub ty: Box<BareFnTy>,
        }),
        /// The never type (`!`)
        pub Never(TyNever {
            pub bang_token: tokens::Bang,
        }),
        /// A tuple (`(A, B, C, D, ...)`)
        pub Tup(TyTup {
            pub paren_token: tokens::Paren,
            pub tys: Delimited<Ty, tokens::Comma>,
            pub lone_comma: Option<tokens::Comma>,
        }),
        /// A path (`module::module::...::Type`), optionally
        /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
        ///
        /// Type parameters are stored in the Path itself
        pub Path(TyPath {
            pub qself: Option<QSelf>,
            pub path: Path,
        }),
        /// A trait object type `Bound1 + Bound2 + Bound3`
        /// where `Bound` is a trait or a lifetime.
        pub TraitObject(TyTraitObject {
            pub bounds: Delimited<TyParamBound, tokens::Add>,
        }),
        /// An `impl Bound1 + Bound2 + Bound3` type
        /// where `Bound` is a trait or a lifetime.
        pub ImplTrait(TyImplTrait {
            pub impl_token: tokens::Impl,
            pub bounds: Delimited<TyParamBound, tokens::Add>,
        }),
        /// No-op; kept solely so that we can pretty-print faithfully
        pub Paren(TyParen {
            pub paren_token: tokens::Paren,
            pub ty: Box<Ty>,
        }),
        /// TyKind::Infer means the type should be inferred instead of it having been
        /// specified. This can appear anywhere in a type.
        pub Infer(TyInfer {
            pub underscore_token: tokens::Underscore
        }),
        /// A macro in the type position.
        pub Mac(Mac),
    }
}

ast_struct! {
    pub struct MutTy {
        pub ty: Ty,
        pub mutability: Mutability,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Mutability {
        Mutable(tokens::Mut),
        Immutable,
    }
}

ast_struct! {
    /// A "Path" is essentially Rust's notion of a name.
    ///
    /// It's represented as a sequence of identifiers,
    /// along with a bunch of supporting information.
    ///
    /// E.g. `std::cmp::PartialEq`
    pub struct Path {
        /// A `::foo` path, is relative to the crate root rather than current
        /// module (like paths in an import).
        pub global: bool,
        pub leading_colon: Option<tokens::Colon2>,
        /// The segments in the path: the things separated by `::`.
        pub segments: Delimited<PathSegment, tokens::Colon2>,
    }
}

#[cfg(feature = "printing")]
ast_struct! {
    pub struct PathTokens<'a>(pub &'a Option<QSelf>, pub &'a Path);
}

impl<T> From<T> for Path
    where T: Into<PathSegment>
{
    fn from(segment: T) -> Self {
        Path {
            global: false,
            leading_colon: None,
            segments: vec![(segment.into(), None)].into(),
        }
    }
}

ast_struct! {
    /// A segment of a path: an identifier, an optional lifetime, and a set of types.
    ///
    /// E.g. `std`, `String` or `Box<T>`
    pub struct PathSegment {
        /// The identifier portion of this path segment.
        pub ident: Ident,
        /// Type/lifetime parameters attached to this path. They come in
        /// two flavors: `Path<A,B,C>` and `Path(A,B) -> C`. Note that
        /// this is more than just simple syntactic sugar; the use of
        /// parens affects the region binding rules, so we preserve the
        /// distinction.
        pub parameters: PathParameters,
    }
}

impl<T> From<T> for PathSegment
    where T: Into<Ident>
{
    fn from(ident: T) -> Self {
        PathSegment {
            ident: ident.into(),
            parameters: PathParameters::none(),
        }
    }
}

ast_enum! {
    /// Parameters of a path segment.
    ///
    /// E.g. `<A, B>` as in `Foo<A, B>` or `(A, B)` as in `Foo(A, B)`
    pub enum PathParameters {
        /// The `<'a, A, B, C>` in `foo::bar::baz::<'a, A, B, C>`
        AngleBracketed(AngleBracketedParameterData),
        /// The `(A, B)` and `C` in `Foo(A, B) -> C`
        Parenthesized(ParenthesizedParameterData),
    }
}

impl PathParameters {
    pub fn none() -> Self {
        PathParameters::AngleBracketed(AngleBracketedParameterData::default())
    }

    pub fn is_empty(&self) -> bool {
        match *self {
            PathParameters::AngleBracketed(ref bracketed) => {
                bracketed.lifetimes.is_empty() && bracketed.types.is_empty() &&
                bracketed.bindings.is_empty()
            }
            PathParameters::Parenthesized(_) => false,
        }
    }
}

ast_struct! {
    /// A path like `Foo<'a, T>`
    #[derive(Default)]
    pub struct AngleBracketedParameterData {
        pub lt_token: Option<tokens::Lt>,
        pub gt_token: Option<tokens::Gt>,

        /// The lifetime parameters for this path segment.
        pub lifetimes: Delimited<Lifetime, tokens::Comma>,
        /// The type parameters for this path segment, if present.
        pub types: Delimited<Ty, tokens::Comma>,
        /// Bindings (equality constraints) on associated types, if present.
        ///
        /// E.g., `Foo<A=Bar>`.
        pub bindings: Delimited<TypeBinding, tokens::Comma>,
    }
}

ast_struct! {
    /// Bind a type to an associated type: `A=Foo`.
    pub struct TypeBinding {
        pub ident: Ident,
        pub eq_token: tokens::Eq,
        pub ty: Ty,
    }
}


ast_struct! {
    /// A path like `Foo(A,B) -> C`
    pub struct ParenthesizedParameterData {
        pub paren_token: tokens::Paren,
        /// `(A, B)`
        pub inputs: Delimited<Ty, tokens::Comma>,
        /// `C`
        pub output: FunctionRetTy,
    }
}

ast_struct! {
    pub struct PolyTraitRef {
        /// The `for<'a>` in `for<'a> Foo<&'a T>`
        pub bound_lifetimes: Option<BoundLifetimes>,
        /// The `Foo<&'a T>` in `<'a> Foo<&'a T>`
        pub trait_ref: Path,
    }
}

ast_struct! {
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
    pub struct QSelf {
        pub lt_token: tokens::Lt,
        pub gt_token: tokens::Gt,
        pub as_token: Option<tokens::As>,
        pub ty: Box<Ty>,
        pub position: usize,
    }
}

ast_struct! {
    pub struct BareFnTy {
        pub lifetimes: Option<BoundLifetimes>,
        pub unsafety: Unsafety,
        pub abi: Option<Abi>,
        pub fn_token: tokens::Fn,
        pub paren_token: tokens::Paren,
        pub inputs: Delimited<BareFnArg, tokens::Comma>,
        pub variadic: Option<tokens::Dot3>,
        pub output: FunctionRetTy,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Unsafety {
        Unsafe(tokens::Unsafe),
        Normal,
    }
}

ast_struct! {
    pub struct Abi {
        pub extern_token: tokens::Extern,
        pub kind: AbiKind,
    }
}

ast_enum! {
    pub enum AbiKind {
        Named(Lit),
        Default,
    }
}

ast_struct! {
    /// An argument in a function type.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub struct BareFnArg {
        pub name: Option<(Ident, tokens::Colon)>,
        pub ty: Ty,
    }
}


ast_enum! {
    pub enum FunctionRetTy {
        /// Return type is not specified.
        ///
        /// Functions default to `()` and
        /// closures default to inference. Span points to where return
        /// type would be inserted.
        Default,
        /// Everything else
        Ty(Ty, tokens::RArrow),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {TyParamBound, TraitBoundModifier};
    #[cfg(feature = "full")]
    use ConstExpr;
    #[cfg(feature = "full")]
    use constant::parsing::const_expr;
    #[cfg(feature = "full")]
    use expr::parsing::expr;
    use generics::parsing::{lifetime, ty_param_bound, bound_lifetimes};
    use ident::parsing::ident;
    use lit::parsing::string;
    use mac::parsing::mac;
    use std::str;

    named!(pub ty -> Ty, alt!(
        ty_paren // must be before ty_tup
        |
        ty_mac // must be before ty_path
        |
        ty_path // must be before ty_poly_trait_ref
        |
        ty_vec
        |
        ty_array
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
        ty_poly_trait_ref
        |
        ty_impl_trait
    ));

    named!(ty_mac -> Ty, map!(mac, Ty::Mac));

    named!(ty_vec -> Ty, do_parse!(
        punct!("[") >>
        elem: ty >>
        punct!("]") >>
        (TySlice {
            ty: Box::new(elem),
            bracket_token: tokens::Bracket::default(),
        }.into())
    ));

    named!(ty_array -> Ty, do_parse!(
        punct!("[") >>
        elem: ty >>
        punct!(";") >>
        len: array_len >>
        punct!("]") >>
        (TyArray {
            ty: Box::new(elem),
            amt: len,
            bracket_token: tokens::Bracket::default(),
            semi_token: tokens::Semi::default(),
        }.into())
    ));

    #[cfg(not(feature = "full"))]
    use constant::parsing::const_expr as array_len;

    #[cfg(feature = "full")]
    named!(array_len -> ConstExpr, alt!(
        terminated!(const_expr, after_array_len)
        |
        terminated!(expr, after_array_len) => { ConstExpr::Other }
    ));

    #[cfg(feature = "full")]
    named!(after_array_len -> &str, peek!(punct!("]")));

    named!(ty_ptr -> Ty, do_parse!(
        punct!("*") >>
        mutability: alt!(
            keyword!("const") => { |_| Mutability::Immutable }
            |
            keyword!("mut") => { |_| Mutability::Mutable(tokens::Mut::default()) }
        ) >>
        target: ty >>
        (TyPtr {
            const_token: match mutability {
                Mutability::Mutable(_) => None,
                Mutability::Immutable => Some(tokens::Const::default()),
            },
            star_token: tokens::Star::default(),
            ty: Box::new(MutTy {
                ty: target,
                mutability: mutability,
            }),
        }.into())
    ));

    named!(ty_rptr -> Ty, do_parse!(
        punct!("&") >>
        life: option!(lifetime) >>
        mutability: mutability >>
        target: ty >>
        (TyRptr {
            lifetime: life,
            ty: Box::new(MutTy {
                ty: target,
                mutability: mutability,
            }),
            and_token: tokens::And::default(),
        }.into())
    ));

    named!(ty_bare_fn -> Ty, do_parse!(
        lifetimes: bound_lifetimes >>
        unsafety: unsafety >>
        abi: option!(abi) >>
        keyword!("fn") >>
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 fn_arg) >>
        variadic: option!(cond_reduce!(inputs.is_empty() || inputs.trailing_delim(),
                                       punct!("..."))) >>
        punct!(")") >>
        output: fn_ret_ty >>
        (TyBareFn {
            ty: Box::new(BareFnTy {
                unsafety: unsafety,
                abi: abi,
                lifetimes: lifetimes,
                inputs: inputs,
                output: output,
                variadic: variadic.map(|_| tokens::Dot3::default()),
                fn_token: tokens::Fn::default(),
                paren_token: tokens::Paren::default(),
            }),
        }.into())
    ));

    named!(ty_never -> Ty, map!(punct!("!"), |_| TyNever {
        bang_token: tokens::Bang::default(),
    }.into()));

    named!(ty_tup -> Ty, do_parse!(
        punct!("(") >>
        elems: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                ty) >>
        punct!(")") >>
        (TyTup {
            tys: elems,
            paren_token: tokens::Paren::default(),
            lone_comma: None, // TODO: does this just not parse?
        }.into())
    ));

    named!(ty_path -> Ty, do_parse!(
        qpath: qpath >>
        parenthesized: cond!(
            qpath.1.segments.get(qpath.1.segments.len() - 1).item().parameters.is_empty(),
            option!(parenthesized_parameter_data)
        ) >>
        bounds: many0!(preceded!(punct!("+"), ty_param_bound)) >>
        ({
            let (qself, mut path) = qpath;
            if let Some(Some(parenthesized)) = parenthesized {
                let len = path.segments.len();
                path.segments.get_mut(len - 1).item_mut().parameters = parenthesized;
            }
            if bounds.is_empty() {
                TyPath { qself: qself, path: path }.into()
            } else {
                let path = TyParamBound::Trait(
                    PolyTraitRef {
                        bound_lifetimes: None,
                        trait_ref: path,
                    },
                    TraitBoundModifier::None,
                );
                let mut new_bounds = Delimited::new();
                new_bounds.push_first(path);
                for bound in bounds {
                    new_bounds.push_default(bound);
                }
                TyTraitObject { bounds: new_bounds }.into()
            }
        })
    ));

    named!(parenthesized_parameter_data -> PathParameters, do_parse!(
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 ty) >>
        punct!(")") >>
        output: fn_ret_ty >>
        (PathParameters::Parenthesized(
            ParenthesizedParameterData {
                paren_token: tokens::Paren::default(),
                inputs: inputs,
                output: output,
            },
        ))
    ));

    named!(pub fn_ret_ty -> FunctionRetTy, map!(
        option!(preceded!(
            punct!("->"),
            ty
        )),
        |ty| {
            match ty {
                Some(ty) => FunctionRetTy::Ty(ty, tokens::RArrow::default()),
                None => FunctionRetTy::Default,
            }
        }
    ));

    named!(pub qpath -> (Option<QSelf>, Path), alt!(
        map!(path, |p| (None, p))
        |
        do_parse!(
            punct!("<") >>
            this: map!(ty, Box::new) >>
            path: option!(preceded!(
                keyword!("as"),
                path
            )) >>
            punct!(">") >>
            punct!("::") >>
            rest: separated_nonempty_list!(
                map!(punct!("::"), |_| tokens::Colon2::default()),
                path_segment
            ) >>
            ({
                let as_token = path.as_ref().map(|_| tokens::As::default());
                let (pos, path) = match path {
                    Some(mut path) => {
                        let pos = path.segments.len();
                        if !path.segments.is_empty() && !path.segments.trailing_delim() {
                            path.segments.push_trailing(tokens::Colon2::default());
                        }
                        for item in rest.into_iter() {
                            path.segments.push(item);
                        }
                        (pos, path)
                    }
                    None => {
                        (0, Path {
                            leading_colon: None,
                            global: false,
                            segments: rest,
                        })
                    }
                };
                (Some(QSelf {
                    ty: this,
                    position: pos,
                    gt_token: tokens::Gt::default(),
                    lt_token: tokens::Lt::default(),
                    as_token: as_token,
                }), path)
            })
        )
        |
        map!(keyword!("self"), |_| (None, "self".into()))
    ));

    named!(ty_poly_trait_ref -> Ty, map!(
        separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                 ty_param_bound),
        |x| TyTraitObject { bounds: x }.into()
    ));

    named!(ty_impl_trait -> Ty, do_parse!(
        keyword!("impl") >>
        elem: separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                       ty_param_bound) >>
        (TyImplTrait {
            impl_token: tokens::Impl::default(),
            bounds: elem,
        }.into())
    ));

    named!(ty_paren -> Ty, do_parse!(
        punct!("(") >>
        elem: ty >>
        punct!(")") >>
        (TyParen {
            paren_token: tokens::Paren::default(),
            ty: Box::new(elem),
        }.into())
    ));

    named!(pub mutability -> Mutability, alt!(
        keyword!("mut") => { |_| Mutability::Mutable(tokens::Mut::default()) }
        |
        epsilon!() => { |_| Mutability::Immutable }
    ));

    named!(pub path -> Path, do_parse!(
        global: option!(punct!("::")) >>
        segments: separated_nonempty_list!(map!(punct!("::"), |_| tokens::Colon2::default()),
                                           path_segment) >>
        (Path {
            global: global.is_some(),
            segments: segments,
            leading_colon: global.map(|_| tokens::Colon2::default()),
        })
    ));

    named!(path_segment -> PathSegment, alt!(
        do_parse!(
            id: option!(ident) >>
            punct!("<") >>
            lifetimes: terminated_list!(
                map!(punct!(","), |_| tokens::Comma::default()),
                lifetime
            ) >>
            types: cond!(
                lifetimes.is_empty() || lifetimes.trailing_delim(),
                terminated_list!(
                    map!(punct!(","), |_| tokens::Comma::default()),
                    terminated!(ty, not!(punct!("=")))
                )
            ) >>
            bindings: cond!(
                match types {
                    Some(ref t) => t.is_empty() || t.trailing_delim(),
                    None => lifetimes.is_empty() || lifetimes.trailing_delim(),
                },
                terminated_list!(
                    map!(punct!(","), |_| tokens::Comma::default()),
                    type_binding
                )
            ) >>
            punct!(">") >>
            (PathSegment {
                ident: id.unwrap_or_else(|| "".into()),
                parameters: PathParameters::AngleBracketed(
                    AngleBracketedParameterData {
                        gt_token: Some(tokens::Gt::default()),
                        lt_token: Some(tokens::Lt::default()),
                        lifetimes: lifetimes,
                        types: types.unwrap_or_default(),
                        bindings: bindings.unwrap_or_default(),
                    }
                ),
            })
        )
        |
        map!(ident, Into::into)
        |
        map!(alt!(
            keyword!("super")
            |
            keyword!("self")
            |
            keyword!("Self")
        ), Into::into)
    ));

    named!(pub mod_style_path -> Path, do_parse!(
        global: option!(punct!("::")) >>
        segments: separated_nonempty_list!(map!(punct!("::"), |_| tokens::Colon2::default()),
                                           mod_style_path_segment) >>
        (Path {
            global: global.is_some(),
            segments: segments,
            leading_colon: global.map(|_| tokens::Colon2::default()),
        })
    ));

    named!(mod_style_path_segment -> PathSegment, alt!(
        map!(ident, Into::into)
        |
        map!(alt!(
            keyword!("super")
            |
            keyword!("self")
            |
            keyword!("Self")
        ), Into::into)
    ));

    named!(type_binding -> TypeBinding, do_parse!(
        id: ident >>
        punct!("=") >>
        ty: ty >>
        (TypeBinding {
            ident: id,
            eq_token: tokens::Eq::default(),
            ty: ty,
        })
    ));

    named!(pub poly_trait_ref -> PolyTraitRef, do_parse!(
        bound_lifetimes: bound_lifetimes >>
        trait_ref: path >>
        parenthesized: option!(cond_reduce!(
            trait_ref.segments.get(trait_ref.segments.len() - 1).item().parameters.is_empty(),
            parenthesized_parameter_data
        )) >>
        ({
            let mut trait_ref = trait_ref;
            if let Some(parenthesized) = parenthesized {
                let len = trait_ref.segments.len();
                trait_ref.segments.get_mut(len - 1).item_mut().parameters = parenthesized;
            }
            PolyTraitRef {
                bound_lifetimes: bound_lifetimes,
                trait_ref: trait_ref,
            }
        })
    ));

    named!(pub fn_arg -> BareFnArg, do_parse!(
        name: option!(do_parse!(
            name: ident >>
            punct!(":") >>
            not!(tag!(":")) >> // not ::
            (name)
        )) >>
        ty: ty >>
        (BareFnArg {
            name: name.map(|t| (t, tokens::Colon::default())),
            ty: ty,
        })
    ));

    named!(pub unsafety -> Unsafety, alt!(
        keyword!("unsafe") => { |_| Unsafety::Unsafe(tokens::Unsafe::default()) }
        |
        epsilon!() => { |_| Unsafety::Normal }
    ));

    named!(pub abi -> Abi, do_parse!(
        keyword!("extern") >>
        name: option!(string) >>
        (Abi {
            extern_token: tokens::Extern::default(),
            kind: match name {
                Some(name) => AbiKind::Named(name),
                None => AbiKind::Default,
            },
        })
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for TySlice {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.ty.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TyArray {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.ty.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.amt.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TyPtr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.star_token.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ty.mutability.to_tokens(tokens);
            self.ty.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for TyRptr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.lifetime.to_tokens(tokens);
            self.ty.mutability.to_tokens(tokens);
            self.ty.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for TyBareFn {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ty.to_tokens(tokens)
        }
    }

    impl ToTokens for TyNever {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bang_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TyTup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.tys.to_tokens(tokens);
                self.lone_comma.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for TyPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            PathTokens(&self.qself, &self.path).to_tokens(tokens);
        }
    }

    impl<'a> ToTokens for PathTokens<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let qself = match *self.0 {
                Some(ref qself) => qself,
                None => return self.1.to_tokens(tokens),
            };
            qself.lt_token.to_tokens(tokens);
            qself.ty.to_tokens(tokens);
            if qself.position > 0 {
                qself.as_token.to_tokens(tokens);
                self.1.leading_colon.to_tokens(tokens);
                for (i, segment) in self.1.segments
                        .iter()
                        .take(qself.position)
                        .enumerate() {
                    if i == qself.position - 1 {
                        segment.item().to_tokens(tokens);
                        qself.gt_token.to_tokens(tokens);
                        segment.delimiter().to_tokens(tokens);
                    } else {
                        segment.to_tokens(tokens);
                    }
                }
            } else {
                qself.gt_token.to_tokens(tokens);
            }
            for segment in self.1.segments.iter().skip(qself.position) {
                segment.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TyTraitObject {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for TyImplTrait {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.impl_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for TyParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.ty.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TyInfer {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.underscore_token.to_tokens(tokens);
        }
    }

    impl ToTokens for Mutability {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if let Mutability::Mutable(ref t) = *self {
                t.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for Path {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.leading_colon.to_tokens(tokens);
            self.segments.to_tokens(tokens);
        }
    }

    impl ToTokens for PathSegment {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.parameters.to_tokens(tokens);
        }
    }

    impl ToTokens for PathParameters {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                PathParameters::AngleBracketed(ref parameters) => {
                    parameters.to_tokens(tokens);
                }
                PathParameters::Parenthesized(ref parameters) => {
                    parameters.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for AngleBracketedParameterData {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lt_token.to_tokens(tokens);
            self.lifetimes.to_tokens(tokens);
            self.types.to_tokens(tokens);
            self.bindings.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeBinding {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ParenthesizedParameterData {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
            });
            self.output.to_tokens(tokens);
        }
    }

    impl ToTokens for FunctionRetTy {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                FunctionRetTy::Default => {}
                FunctionRetTy::Ty(ref ty, ref arrow) => {
                    arrow.to_tokens(tokens);
                    ty.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for PolyTraitRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bound_lifetimes.to_tokens(tokens);
            self.trait_ref.to_tokens(tokens);
        }
    }

    impl ToTokens for BareFnTy {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lifetimes.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.abi.to_tokens(tokens);
            self.fn_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
                self.variadic.to_tokens(tokens);
            });
            self.output.to_tokens(tokens);
        }
    }

    impl ToTokens for BareFnArg {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if let Some((ref name, ref colon)) = self.name {
                name.to_tokens(tokens);
                colon.to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for Unsafety {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Unsafety::Unsafe(ref t) => t.to_tokens(tokens),
                Unsafety::Normal => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for Abi {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.extern_token.to_tokens(tokens);
            match self.kind {
                AbiKind::Named(ref named) => named.to_tokens(tokens),
                AbiKind::Default => {}
            }
        }
    }
}
