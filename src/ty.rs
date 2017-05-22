use super::*;

ast_enum_of_structs! {
    /// The different kinds of types recognized by the compiler
    pub enum Ty {
        /// A variable-length array (`[T]`)
        pub Slice(TySlice {
            pub ty: Box<Ty>,
        }),
        /// A fixed length array (`[T; n]`)
        pub Array(TyArray {
            pub ty: Box<Ty>,
            pub amt: ConstExpr,
        }),
        /// A raw pointer (`*const T` or `*mut T`)
        pub Ptr(TyPtr {
            pub ty: Box<MutTy>,
        }),
        /// A reference (`&'a T` or `&'a mut T`)
        pub Rptr(TyRptr {
            pub lifetime: Option<Lifetime>,
            pub ty: Box<MutTy>,
        }),
        /// A bare function (e.g. `fn(usize) -> bool`)
        pub BareFn(TyBareFn {
            pub ty: Box<BareFnTy>,
        }),
        /// The never type (`!`)
        pub Never(TyNever {}),
        /// A tuple (`(A, B, C, D, ...)`)
        pub Tup(TyTup {
            pub tys: Vec<Ty>,
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
            pub bounds: Vec<TyParamBound>,
        }),
        /// An `impl Bound1 + Bound2 + Bound3` type
        /// where `Bound` is a trait or a lifetime.
        pub ImplTrait(TyImplTrait {
            pub bounds: Vec<TyParamBound>,
        }),
        /// No-op; kept solely so that we can pretty-print faithfully
        pub Paren(TyParen {
            pub ty: Box<Ty>,
        }),
        /// TyKind::Infer means the type should be inferred instead of it having been
        /// specified. This can appear anywhere in a type.
        pub Infer(TyInfer {}),
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
    #[derive(Copy)]
    pub enum Mutability {
        Mutable,
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
        /// The segments in the path: the things separated by `::`.
        pub segments: Vec<PathSegment>,
    }
}

impl<T> From<T> for Path
    where T: Into<PathSegment>
{
    fn from(segment: T) -> Self {
        Path {
            global: false,
            segments: vec![segment.into()],
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
        /// The lifetime parameters for this path segment.
        pub lifetimes: Vec<Lifetime>,
        /// The type parameters for this path segment, if present.
        pub types: Vec<Ty>,
        /// Bindings (equality constraints) on associated types, if present.
        ///
        /// E.g., `Foo<A=Bar>`.
        pub bindings: Vec<TypeBinding>,
    }
}

ast_struct! {
    /// Bind a type to an associated type: `A=Foo`.
    pub struct TypeBinding {
        pub ident: Ident,
        pub ty: Ty,
    }
}


ast_struct! {
    /// A path like `Foo(A,B) -> C`
    pub struct ParenthesizedParameterData {
        /// `(A, B)`
        pub inputs: Vec<Ty>,
        /// `C`
        pub output: Option<Ty>,
    }
}

ast_struct! {
    pub struct PolyTraitRef {
        /// The `'a` in `<'a> Foo<&'a T>`
        pub bound_lifetimes: Vec<LifetimeDef>,
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
        pub ty: Box<Ty>,
        pub position: usize,
    }
}

ast_struct! {
    pub struct BareFnTy {
        pub unsafety: Unsafety,
        pub abi: Option<Abi>,
        pub lifetimes: Vec<LifetimeDef>,
        pub inputs: Vec<BareFnArg>,
        pub output: FunctionRetTy,
        pub variadic: bool,
    }
}

ast_enum! {
    #[derive(Copy)]
    pub enum Unsafety {
        Unsafe,
        Normal,
    }
}

ast_enum! {
    pub enum Abi {
        Named(String),
        Rust,
    }
}

ast_struct! {
    /// An argument in a function type.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub struct BareFnArg {
        pub name: Option<Ident>,
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
        Ty(Ty),
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
    use generics::parsing::{lifetime, lifetime_def, ty_param_bound, bound_lifetimes};
    use ident::parsing::ident;
    use lit::parsing::quoted_string;
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
        (TySlice { ty: Box::new(elem) }.into())
    ));

    named!(ty_array -> Ty, do_parse!(
        punct!("[") >>
        elem: ty >>
        punct!(";") >>
        len: array_len >>
        punct!("]") >>
        (TyArray { ty: Box::new(elem), amt: len }.into())
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
            keyword!("mut") => { |_| Mutability::Mutable }
        ) >>
        target: ty >>
        (TyPtr {
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
        }.into())
    ));

    named!(ty_bare_fn -> Ty, do_parse!(
        lifetimes: opt_vec!(do_parse!(
            keyword!("for") >>
            punct!("<") >>
            lifetimes: terminated_list!(punct!(","), lifetime_def) >>
            punct!(">") >>
            (lifetimes)
        )) >>
        unsafety: unsafety >>
        abi: option!(abi) >>
        keyword!("fn") >>
        punct!("(") >>
        inputs: separated_list!(punct!(","), fn_arg) >>
        trailing_comma: option!(punct!(",")) >>
        variadic: option!(cond_reduce!(trailing_comma.is_some(), punct!("..."))) >>
        punct!(")") >>
        output: option!(preceded!(
            punct!("->"),
            ty
        )) >>
        (TyBareFn {
            ty: Box::new(BareFnTy {
                unsafety: unsafety,
                abi: abi,
                lifetimes: lifetimes,
                inputs: inputs,
                output: match output {
                    Some(ty) => FunctionRetTy::Ty(ty),
                    None => FunctionRetTy::Default,
                },
                variadic: variadic.is_some(),
            }),
        }.into())
    ));

    named!(ty_never -> Ty, map!(punct!("!"), |_| TyNever {}.into()));

    named!(ty_tup -> Ty, do_parse!(
        punct!("(") >>
        elems: terminated_list!(punct!(","), ty) >>
        punct!(")") >>
        (TyTup { tys: elems }.into())
    ));

    named!(ty_path -> Ty, do_parse!(
        qpath: qpath >>
        parenthesized: cond!(
            qpath.1.segments.last().unwrap().parameters == PathParameters::none(),
            option!(parenthesized_parameter_data)
        ) >>
        bounds: many0!(preceded!(punct!("+"), ty_param_bound)) >>
        ({
            let (qself, mut path) = qpath;
            if let Some(Some(parenthesized)) = parenthesized {
                path.segments.last_mut().unwrap().parameters = parenthesized;
            }
            if bounds.is_empty() {
                TyPath { qself: qself, path: path }.into()
            } else {
                let path = TyParamBound::Trait(
                    PolyTraitRef {
                        bound_lifetimes: Vec::new(),
                        trait_ref: path,
                    },
                    TraitBoundModifier::None,
                );
                let bounds = Some(path).into_iter().chain(bounds).collect();
                TyTraitObject { bounds: bounds }.into()
            }
        })
    ));

    named!(parenthesized_parameter_data -> PathParameters, do_parse!(
        punct!("(") >>
        inputs: terminated_list!(punct!(","), ty) >>
        punct!(")") >>
        output: option!(preceded!(
            punct!("->"),
            ty
        )) >>
        (PathParameters::Parenthesized(
            ParenthesizedParameterData {
                inputs: inputs,
                output: output,
            },
        ))
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
            rest: separated_nonempty_list!(punct!("::"), path_segment) >>
            ({
                match path {
                    Some(mut path) => {
                        let pos = path.segments.len();
                        path.segments.extend(rest);
                        (Some(QSelf { ty: this, position: pos }), path)
                    }
                    None => {
                        (Some(QSelf { ty: this, position: 0 }), Path {
                            global: false,
                            segments: rest,
                        })
                    }
                }
            })
        )
        |
        map!(keyword!("self"), |_| (None, "self".into()))
    ));

    named!(ty_poly_trait_ref -> Ty, map!(
        separated_nonempty_list!(punct!("+"), ty_param_bound),
        |x| TyTraitObject { bounds: x }.into()
    ));

    named!(ty_impl_trait -> Ty, do_parse!(
        keyword!("impl") >>
        elem: separated_nonempty_list!(punct!("+"), ty_param_bound) >>
        (TyImplTrait { bounds: elem }.into())
    ));

    named!(ty_paren -> Ty, do_parse!(
        punct!("(") >>
        elem: ty >>
        punct!(")") >>
        (TyParen { ty: Box::new(elem) }.into())
    ));

    named!(pub mutability -> Mutability, alt!(
        keyword!("mut") => { |_| Mutability::Mutable }
        |
        epsilon!() => { |_| Mutability::Immutable }
    ));

    named!(pub path -> Path, do_parse!(
        global: option!(punct!("::")) >>
        segments: separated_nonempty_list!(punct!("::"), path_segment) >>
        (Path {
            global: global.is_some(),
            segments: segments,
        })
    ));

    named!(path_segment -> PathSegment, alt!(
        do_parse!(
            id: option!(ident) >>
            punct!("<") >>
            lifetimes: separated_list!(punct!(","), lifetime) >>
            types: opt_vec!(preceded!(
                cond!(!lifetimes.is_empty(), punct!(",")),
                separated_nonempty_list!(
                    punct!(","),
                    terminated!(ty, not!(punct!("=")))
                )
            )) >>
            bindings: opt_vec!(preceded!(
                cond!(!lifetimes.is_empty() || !types.is_empty(), punct!(",")),
                separated_nonempty_list!(punct!(","), type_binding)
            )) >>
            cond!(!lifetimes.is_empty() || !types.is_empty() || !bindings.is_empty(), option!(punct!(","))) >>
            punct!(">") >>
            (PathSegment {
                ident: id.unwrap_or_else(|| "".into()),
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
        segments: separated_nonempty_list!(punct!("::"), mod_style_path_segment) >>
        (Path {
            global: global.is_some(),
            segments: segments,
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
            ty: ty,
        })
    ));

    named!(pub poly_trait_ref -> PolyTraitRef, do_parse!(
        bound_lifetimes: bound_lifetimes >>
        trait_ref: path >>
        parenthesized: option!(cond_reduce!(
            trait_ref.segments.last().unwrap().parameters == PathParameters::none(),
            parenthesized_parameter_data
        )) >>
        ({
            let mut trait_ref = trait_ref;
            if let Some(parenthesized) = parenthesized {
                trait_ref.segments.last_mut().unwrap().parameters = parenthesized;
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
            name: name,
            ty: ty,
        })
    ));

    named!(pub unsafety -> Unsafety, alt!(
        keyword!("unsafe") => { |_| Unsafety::Unsafe }
        |
        epsilon!() => { |_| Unsafety::Normal }
    ));

    named!(pub abi -> Abi, do_parse!(
        keyword!("extern") >>
        name: option!(quoted_string) >>
        (match name {
            Some(name) => Abi::Named(name),
            None => Abi::Rust,
        })
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for TySlice {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("[");
            self.ty.to_tokens(tokens);
            tokens.append("]");
        }
    }

    impl ToTokens for TyArray {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("[");
            self.ty.to_tokens(tokens);
            tokens.append(";");
            self.amt.to_tokens(tokens);
            tokens.append("]");
        }
    }

    impl ToTokens for TyPtr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("*");
            match self.ty.mutability {
                Mutability::Mutable => tokens.append("mut"),
                Mutability::Immutable => tokens.append("const"),
            }
            self.ty.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for TyRptr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("&");
            self.lifetime.to_tokens(tokens);
            match self.ty.mutability {
                Mutability::Mutable => tokens.append("mut"),
                Mutability::Immutable => {}
            }
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
            tokens.append("!");
        }
    }

    impl ToTokens for TyTup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("(");
            tokens.append_separated(&self.tys, ",");
            if self.tys.len() == 1 {
                tokens.append(",");
            }
            tokens.append(")");
        }
    }

    impl ToTokens for TyPath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let qself = match self.qself {
                Some(ref qself) => qself,
                None => return self.path.to_tokens(tokens),
            };
            tokens.append("<");
            qself.ty.to_tokens(tokens);
            if qself.position > 0 {
                tokens.append("as");
                for (i, segment) in self.path.segments
                        .iter()
                        .take(qself.position)
                        .enumerate() {
                    if i > 0 || self.path.global {
                        tokens.append("::");
                    }
                    segment.to_tokens(tokens);
                }
            }
            tokens.append(">");
            for segment in self.path.segments.iter().skip(qself.position) {
                tokens.append("::");
                segment.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TyTraitObject {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_separated(&self.bounds, "+");
        }
    }

    impl ToTokens for TyImplTrait {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("impl");
            tokens.append_separated(&self.bounds, "+");
        }
    }

    impl ToTokens for TyParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("(");
            self.ty.to_tokens(tokens);
            tokens.append(")");
        }
    }

    impl ToTokens for TyInfer {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("_");
        }
    }

    impl ToTokens for Mutability {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if let Mutability::Mutable = *self {
                tokens.append("mut");
            }
        }
    }

    impl ToTokens for Path {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for (i, segment) in self.segments.iter().enumerate() {
                if i > 0 || self.global {
                    tokens.append("::");
                }
                segment.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for PathSegment {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            if self.ident.as_ref().is_empty() && self.parameters.is_empty() {
                tokens.append("<");
                tokens.append(">");
            } else {
                self.parameters.to_tokens(tokens);
            }
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
            let has_lifetimes = !self.lifetimes.is_empty();
            let has_types = !self.types.is_empty();
            let has_bindings = !self.bindings.is_empty();
            if !has_lifetimes && !has_types && !has_bindings {
                return;
            }

            tokens.append("<");

            let mut first = true;
            for lifetime in &self.lifetimes {
                if !first {
                    tokens.append(",");
                }
                lifetime.to_tokens(tokens);
                first = false;
            }
            for ty in &self.types {
                if !first {
                    tokens.append(",");
                }
                ty.to_tokens(tokens);
                first = false;
            }
            for binding in &self.bindings {
                if !first {
                    tokens.append(",");
                }
                binding.to_tokens(tokens);
                first = false;
            }

            tokens.append(">");
        }
    }

    impl ToTokens for TypeBinding {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            tokens.append("=");
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ParenthesizedParameterData {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("(");
            tokens.append_separated(&self.inputs, ",");
            tokens.append(")");
            if let Some(ref output) = self.output {
                tokens.append("->");
                output.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for PolyTraitRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if !self.bound_lifetimes.is_empty() {
                tokens.append("for");
                tokens.append("<");
                tokens.append_separated(&self.bound_lifetimes, ",");
                tokens.append(">");
            }
            self.trait_ref.to_tokens(tokens);
        }
    }

    impl ToTokens for BareFnTy {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if !self.lifetimes.is_empty() {
                tokens.append("for");
                tokens.append("<");
                tokens.append_separated(&self.lifetimes, ",");
                tokens.append(">");
            }
            self.unsafety.to_tokens(tokens);
            self.abi.to_tokens(tokens);
            tokens.append("fn");
            tokens.append("(");
            tokens.append_separated(&self.inputs, ",");
            if self.variadic {
                if !self.inputs.is_empty() {
                    tokens.append(",");
                }
                tokens.append("...");
            }
            tokens.append(")");
            if let FunctionRetTy::Ty(ref ty) = self.output {
                tokens.append("->");
                ty.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for BareFnArg {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if let Some(ref name) = self.name {
                name.to_tokens(tokens);
                tokens.append(":");
            }
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for Unsafety {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Unsafety::Unsafe => tokens.append("unsafe"),
                Unsafety::Normal => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for Abi {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("extern");
            match *self {
                Abi::Named(ref named) => named.to_tokens(tokens),
                Abi::Rust => {}
            }
        }
    }
}
