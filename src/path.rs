use punctuated::Punctuated;
use super::*;

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
        pub leading_colon: Option<Token![::]>,
        /// The segments in the path: the things separated by `::`.
        pub segments: Punctuated<PathSegment, Token![::]>,
    }
}

impl Path {
    pub fn global(&self) -> bool {
        self.leading_colon.is_some()
    }
}

#[cfg(feature = "printing")]
#[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct PathTokens<'a>(pub &'a Option<QSelf>, pub &'a Path);

impl<T> From<T> for Path
where
    T: Into<PathSegment>,
{
    fn from(segment: T) -> Self {
        let mut path = Path {
            leading_colon: None,
            segments: Punctuated::new(),
        };
        path.segments.push_item(segment.into());
        path
    }
}

ast_struct! {
    /// A segment of a path: an identifier, an optional lifetime, and a set of types.
    ///
    /// E.g. `std`, `String` or `Box<T>`
    pub struct PathSegment {
        /// The identifier portion of this path segment.
        pub ident: Ident,
        /// Type/lifetime arguments attached to this path. They come in
        /// two flavors: `Path<A,B,C>` and `Path(A,B) -> C`. Note that
        /// this is more than just simple syntactic sugar; the use of
        /// parens affects the region binding rules, so we preserve the
        /// distinction.
        pub arguments: PathArguments,
    }
}

impl<T> From<T> for PathSegment
where
    T: Into<Ident>,
{
    fn from(ident: T) -> Self {
        PathSegment {
            ident: ident.into(),
            arguments: PathArguments::None,
        }
    }
}

ast_enum! {
    /// Arguments of a path segment.
    ///
    /// E.g. `<A, B>` as in `Foo<A, B>` or `(A, B)` as in `Foo(A, B)`
    pub enum PathArguments {
        None,
        /// The `<'a, A, B, C>` in `foo::bar::baz::<'a, A, B, C>`
        AngleBracketed(AngleBracketedGenericArguments),
        /// The `(A, B)` and `C` in `Foo(A, B) -> C`
        Parenthesized(ParenthesizedGenericArguments),
    }
}

impl Default for PathArguments {
    fn default() -> Self {
        PathArguments::None
    }
}

impl PathArguments {
    pub fn is_empty(&self) -> bool {
        match *self {
            PathArguments::None => true,
            PathArguments::AngleBracketed(ref bracketed) => bracketed.args.is_empty(),
            PathArguments::Parenthesized(_) => false,
        }
    }
}

ast_enum! {
    /// A individual generic argument, like `'a`, `T`, or `Item=T`.
    pub enum GenericArgument {
        /// The lifetime parameters for this path segment.
        Lifetime(Lifetime),
        /// The type parameters for this path segment, if present.
        Type(Type),
        /// Bindings (equality constraints) on associated types, if present.
        ///
        /// E.g., `Foo<A=Bar>`.
        Binding(Binding),
        /// Const expression. Must be inside of a block.
        ///
        /// NOTE: Identity expressions are represented as Type arguments, as
        /// they are indistinguishable syntactically.
        Const(Expr),
    }
}

ast_struct! {
    /// A path like `Foo<'a, T>`
    pub struct AngleBracketedGenericArguments {
        pub colon2_token: Option<Token![::]>,
        pub lt_token: Token![<],
        pub args: Punctuated<GenericArgument, Token![,]>,
        pub gt_token: Token![>],
    }
}

ast_struct! {
    /// Bind a type to an associated type: `A=Foo`.
    pub struct Binding {
        pub ident: Ident,
        pub eq_token: Token![=],
        pub ty: Type,
    }
}

ast_struct! {
    /// A path like `Foo(A,B) -> C`
    pub struct ParenthesizedGenericArguments {
        pub paren_token: token::Paren,
        /// `(A, B)`
        pub inputs: Punctuated<Type, Token![,]>,
        /// `C`
        pub output: ReturnType,
    }
}

ast_struct! {
    /// The explicit Self type in a "qualified path". The actual
    /// path, including the trait and the associated item, is stored
    /// separately. `position` represents the index of the associated
    /// item qualified with this Self type.
    ///
    /// ```text
    /// <Vec<T> as a::b::Trait>::AssociatedItem
    ///  ^~~~~~    ~~~~~~~~~~~~~~^
    ///  ty        position = 3
    ///
    /// <Vec<T>>::AssociatedItem
    ///  ^~~~~~   ^
    ///  ty       position = 0
    /// ```
    pub struct QSelf {
        pub lt_token: Token![<],
        pub ty: Box<Type>,
        pub position: usize,
        pub as_token: Option<Token![as]>,
        pub gt_token: Token![>],
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::Synom;

    impl Synom for Path {
        named!(parse -> Self, do_parse!(
            colon: option!(punct!(::)) >>
            segments: call!(Punctuated::<PathSegment, Token![::]>::parse_separated_nonempty) >>
            cond_reduce!(segments.first().map_or(true, |seg| seg.item().ident != "dyn"), epsilon!()) >>
            (Path {
                leading_colon: colon,
                segments: segments,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("path")
        }
    }

    #[cfg(not(feature = "full"))]
    impl Synom for GenericArgument {
        named!(parse -> Self, alt!(
            call!(ty_no_eq_after) => { GenericArgument::Type }
            |
            syn!(Lifetime) => { GenericArgument::Lifetime }
            |
            syn!(Binding) => { GenericArgument::Binding }
        ));
    }

    #[cfg(feature = "full")]
    impl Synom for GenericArgument {
        named!(parse -> Self, alt!(
            call!(ty_no_eq_after) => { GenericArgument::Type }
            |
            syn!(Lifetime) => { GenericArgument::Lifetime }
            |
            syn!(Binding) => { GenericArgument::Binding }
            |
            syn!(ExprLit) => { |l| GenericArgument::Const(Expr::Lit(l)) }
            |
            syn!(ExprBlock) => { |b| GenericArgument::Const(Expr::Block(b)) }
        ));

        fn description() -> Option<&'static str> {
            Some("generic argument")
        }
    }

    impl Synom for AngleBracketedGenericArguments {
        named!(parse -> Self, do_parse!(
            colon2: option!(punct!(::)) >>
            lt: punct!(<) >>
            args: call!(Punctuated::parse_terminated) >>
            gt: punct!(>) >>
            (AngleBracketedGenericArguments {
                colon2_token: colon2,
                lt_token: lt,
                args: args,
                gt_token: gt,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("angle bracketed generic arguments")
        }
    }

    impl Synom for ParenthesizedGenericArguments {
        named!(parse -> Self, do_parse!(
            data: parens!(Punctuated::parse_terminated) >>
            output: syn!(ReturnType) >>
            (ParenthesizedGenericArguments {
                paren_token: data.0,
                inputs: data.1,
                output: output,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("parenthesized generic arguments: `Foo(A, B, ..) -> T`")
        }
    }

    impl Synom for PathSegment {
        named!(parse -> Self, alt!(
            do_parse!(
                ident: syn!(Ident) >>
                arguments: syn!(AngleBracketedGenericArguments) >>
                (PathSegment {
                    ident: ident,
                    arguments: PathArguments::AngleBracketed(arguments),
                })
            )
            |
            mod_style_path_segment
        ));

        fn description() -> Option<&'static str> {
            Some("path segment")
        }
    }

    impl Synom for Binding {
        named!(parse -> Self, do_parse!(
            id: syn!(Ident) >>
            eq: punct!(=) >>
            ty: syn!(Type) >>
            (Binding {
                ident: id,
                eq_token: eq,
                ty: ty,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("associated type binding")
        }
    }

    impl Path {
        named!(pub parse_mod_style -> Self, do_parse!(
            colon: option!(punct!(::)) >>
            segments: call!(Punctuated::parse_separated_nonempty_with,
                            mod_style_path_segment) >>
            (Path {
                leading_colon: colon,
                segments: segments,
            })
        ));
    }

    named!(mod_style_path_segment -> PathSegment, alt!(
        syn!(Ident) => { Into::into }
        |
        keyword!(super) => { Into::into }
        |
        keyword!(self) => { Into::into }
        |
        keyword!(Self) => { Into::into }
        |
        keyword!(crate) => { Into::into }
    ));

    named!(pub qpath -> (Option<QSelf>, Path), alt!(
        map!(syn!(Path), |p| (None, p))
        |
        do_parse!(
            lt: punct!(<) >>
            this: syn!(Type) >>
            path: option!(tuple!(keyword!(as), syn!(Path))) >>
            gt: punct!(>) >>
            colon2: punct!(::) >>
            rest: call!(Punctuated::parse_separated_nonempty) >>
            ({
                let (pos, as_, path) = match path {
                    Some((as_, mut path)) => {
                        let pos = path.segments.len();
                        path.segments.push_punct(colon2);
                        path.segments.extend(rest.into_elements());
                        (pos, Some(as_), path)
                    }
                    None => {
                        (0, None, Path {
                            leading_colon: Some(colon2),
                            segments: rest,
                        })
                    }
                };
                (Some(QSelf {
                    lt_token: lt,
                    ty: Box::new(this),
                    position: pos,
                    as_token: as_,
                    gt_token: gt,
                }), path)
            })
        )
        |
        map!(keyword!(self), |s| (None, s.into()))
    ));

    named!(pub ty_no_eq_after -> Type, do_parse!(
        ty: syn!(Type) >>
        not!(punct!(=)) >>
        (ty)
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{ToTokens, Tokens};

    impl ToTokens for Path {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.leading_colon.to_tokens(tokens);
            self.segments.to_tokens(tokens);
        }
    }

    impl ToTokens for PathSegment {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.arguments.to_tokens(tokens);
        }
    }

    impl ToTokens for PathArguments {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                PathArguments::None => {}
                PathArguments::AngleBracketed(ref arguments) => {
                    arguments.to_tokens(tokens);
                }
                PathArguments::Parenthesized(ref arguments) => {
                    arguments.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for GenericArgument {
        #[cfg_attr(feature = "cargo-clippy", allow(match_same_arms))]
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                GenericArgument::Lifetime(ref lt) => lt.to_tokens(tokens),
                GenericArgument::Type(ref ty) => ty.to_tokens(tokens),
                GenericArgument::Binding(ref tb) => tb.to_tokens(tokens),
                GenericArgument::Const(ref e) => match *e {
                    Expr::Lit(_) => e.to_tokens(tokens),

                    // NOTE: We should probably support parsing blocks with only
                    // expressions in them without the full feature for const
                    // generics.
                    #[cfg(feature = "full")]
                    Expr::Block(_) => e.to_tokens(tokens),

                    // ERROR CORRECTION: Add braces to make sure that the
                    // generated code is valid.
                    _ => token::Brace::default().surround(tokens, |tokens| {
                        e.to_tokens(tokens);
                    }),
                },
            }
        }
    }

    impl ToTokens for AngleBracketedGenericArguments {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.colon2_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.args.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    impl ToTokens for Binding {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ParenthesizedGenericArguments {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
            });
            self.output.to_tokens(tokens);
        }
    }
}
