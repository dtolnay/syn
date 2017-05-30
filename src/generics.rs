use super::*;
use delimited::Delimited;

ast_struct! {
    /// Represents lifetimes and type parameters attached to a declaration
    /// of a function, enum, trait, etc.
    #[derive(Default)]
    pub struct Generics {
        pub lt_token: Option<tokens::Lt>,
        pub gt_token: Option<tokens::Gt>,
        pub lifetimes: Delimited<LifetimeDef, tokens::Comma>,
        pub ty_params: Delimited<TyParam, tokens::Comma>,
        pub where_clause: WhereClause,
    }
}

#[cfg(feature = "printing")]
ast_struct! {
    /// Returned by `Generics::split_for_impl`.
    pub struct ImplGenerics<'a>(&'a Generics);
}

#[cfg(feature = "printing")]
ast_struct! {
    /// Returned by `Generics::split_for_impl`.
    pub struct TyGenerics<'a>(&'a Generics);
}

#[cfg(feature = "printing")]
ast_struct! {
    /// Returned by `TyGenerics::as_turbofish`.
    pub struct Turbofish<'a>(&'a Generics);
}

#[cfg(feature = "printing")]
impl Generics {
    /// Split a type's generics into the pieces required for impl'ing a trait
    /// for that type.
    ///
    /// ```
    /// # extern crate syn;
    /// # #[macro_use]
    /// # extern crate quote;
    /// # fn main() {
    /// # let generics: syn::Generics = Default::default();
    /// # let name = syn::Ident::from("MyType");
    /// let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    /// quote! {
    ///     impl #impl_generics MyTrait for #name #ty_generics #where_clause {
    ///         // ...
    ///     }
    /// }
    /// # ;
    /// # }
    /// ```
    pub fn split_for_impl(&self) -> (ImplGenerics, TyGenerics, &WhereClause) {
        (ImplGenerics(self), TyGenerics(self), &self.where_clause)
    }
}

#[cfg(feature = "printing")]
impl<'a> TyGenerics<'a> {
    /// Turn a type's generics like `<X, Y>` into a turbofish like `::<X, Y>`.
    pub fn as_turbofish(&self) -> Turbofish {
        Turbofish(self.0)
    }
}

ast_struct! {
    pub struct Lifetime {
        pub ident: Ident,
    }
}

impl Lifetime {
    pub fn new<T: Into<Ident>>(t: T) -> Self {
        let id = t.into();
        if !id.as_ref().starts_with('\'') {
            panic!("lifetime name must start with apostrophe as in \"'a\", \
                   got {:?}",
                   id.as_ref());
        }
        Lifetime { ident: id }
    }
}

ast_struct! {
    /// A set of bound lifetimes, e.g. `for<'a, 'b, 'c>`
    #[derive(Default)]
    pub struct BoundLifetimes {
        pub for_token: tokens::For,
        pub lt_token: tokens::Lt,
        pub lifetimes: Delimited<LifetimeDef, tokens::Comma>,
        pub gt_token: tokens::Gt,
    }
}

ast_struct! {
    /// A lifetime definition, e.g. `'a: 'b+'c+'d`
    pub struct LifetimeDef {
        pub attrs: Vec<Attribute>,
        pub lifetime: Lifetime,
        pub colon_token: Option<tokens::Colon>,
        pub bounds: Delimited<Lifetime, tokens::Add>,
    }
}

impl LifetimeDef {
    pub fn new<T: Into<Ident>>(t: T) -> Self {
        LifetimeDef {
            attrs: Vec::new(),
            lifetime: Lifetime::new(t),
            colon_token: None,
            bounds: Delimited::new(),
        }
    }
}

ast_struct! {
    /// A generic type parameter, e.g. `T: Into<String>`.
    pub struct TyParam {
        pub attrs: Vec<Attribute>,
        pub ident: Ident,
        pub colon_token: Option<tokens::Colon>,
        pub bounds: Delimited<TyParamBound, tokens::Add>,
        pub eq_token: Option<tokens::Eq>,
        pub default: Option<Ty>,
    }
}

impl From<Ident> for TyParam {
    fn from(ident: Ident) -> Self {
        TyParam {
            attrs: vec![],
            ident: ident,
            colon_token: None,
            bounds: Delimited::new(),
            eq_token: None,
            default: None,
        }
    }
}

ast_enum! {
    /// The AST represents all type param bounds as types.
    /// `typeck::collect::compute_bounds` matches these against
    /// the "special" built-in traits (see `middle::lang_items`) and
    /// detects Copy, Send and Sync.
    pub enum TyParamBound {
        Trait(PolyTraitRef, TraitBoundModifier),
        Region(Lifetime),
    }
}

ast_enum! {
    /// A modifier on a bound, currently this is only used for `?Sized`, where the
    /// modifier is `Maybe`. Negative bounds should also be handled here.
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum TraitBoundModifier {
        None,
        Maybe(tokens::Question),
    }
}

ast_struct! {
    /// A `where` clause in a definition
    #[derive(Default)]
    pub struct WhereClause {
        pub where_token: Option<tokens::Where>,
        pub predicates: Delimited<WherePredicate, tokens::Comma>,
    }
}

impl WhereClause {
    pub fn none() -> Self {
        WhereClause::default()
    }
}

ast_enum_of_structs! {
    /// A single predicate in a `where` clause
    pub enum WherePredicate {
        /// A type binding, e.g. `for<'c> Foo: Send+Clone+'c`
        pub BoundPredicate(WhereBoundPredicate {
            /// Any lifetimes from a `for` binding
            pub bound_lifetimes: Option<BoundLifetimes>,
            /// The type being bounded
            pub bounded_ty: Ty,
            pub colon_token: tokens::Colon,
            /// Trait and lifetime bounds (`Clone+Send+'static`)
            pub bounds: Delimited<TyParamBound, tokens::Add>,
        }),

        /// A lifetime predicate, e.g. `'a: 'b+'c`
        pub RegionPredicate(WhereRegionPredicate {
            pub lifetime: Lifetime,
            pub colon_token: Option<tokens::Colon>,
            pub bounds: Delimited<Lifetime, tokens::Add>,
        }),

        /// An equality predicate (unsupported)
        pub EqPredicate(WhereEqPredicate {
            pub lhs_ty: Ty,
            pub eq_token: tokens::Eq,
            pub rhs_ty: Ty,
        }),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use attr::parsing::outer_attr;
    use ident::parsing::ident;
    use ty::parsing::{ty, poly_trait_ref};
    use synom::{TokenTree, IResult};

    named!(pub generics -> Generics, map!(
        alt!(
            do_parse!(
                punct!("<") >>
                lifetimes: terminated_list!(
                    map!(punct!(","), |_| tokens::Comma::default()),
                    lifetime_def
                ) >>
                ty_params: cond!(
                    lifetimes.is_empty() || lifetimes.trailing_delim(),
                    terminated_list!(
                        map!(punct!(","), |_| tokens::Comma::default()),
                        ty_param
                    )
                ) >>
                punct!(">") >>
                (lifetimes, ty_params, true)
            )
            |
            epsilon!() => { |_| (Delimited::new(), None, false) }
        ),
        |(lifetimes, ty_params, any): (_, Option<_>, _)| Generics {
            lifetimes: lifetimes,
            ty_params: ty_params.unwrap_or_default(),
            where_clause: WhereClause::default(),
            gt_token: if any {Some(tokens::Gt::default())} else {None},
            lt_token: if any {Some(tokens::Lt::default())} else {None},
        }
    ));

    pub fn lifetime(input: &[TokenTree]) -> IResult<&[TokenTree], Lifetime> {
        use synom::*;
        if let Some(&TokenTree { kind: TokenKind::Word(ref id), .. }) = input.first() {
            // Check if this word is _actually_ a lifetime, and treat that differently
            if id.chars().next().unwrap() == '\'' {
                IResult::Done(&input[1..], Lifetime {
                    ident: id.to_string().into()
                })
            } else {
                IResult::Error
            }
        } else {
            IResult::Error
        }
    }

    named!(pub lifetime_def -> LifetimeDef, do_parse!(
        attrs: many0!(outer_attr) >>
        life: lifetime >>
        colon: option!(punct!(":")) >>
        bounds: cond!(
            colon.is_some(),
            separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                     lifetime)
        ) >>
        (LifetimeDef {
            attrs: attrs,
            lifetime: life,
            bounds: bounds.unwrap_or_default(),
            colon_token: colon.map(|_| tokens::Colon::default()),
        })
    ));

    named!(pub bound_lifetimes -> Option<BoundLifetimes>, option!(do_parse!(
        keyword!("for") >>
        punct!("<") >>
        lifetimes: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                    lifetime_def) >>
        punct!(">") >>
        (BoundLifetimes {
            for_token: tokens::For::default(),
            lt_token: tokens::Lt::default(),
            gt_token: tokens::Gt::default(),
            lifetimes: lifetimes,
        })
    )));

    named!(ty_param -> TyParam, do_parse!(
        attrs: many0!(outer_attr) >>
        id: ident >>
        colon: option!(punct!(":")) >>
        bounds: cond!(
            colon.is_some(),
            separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                     ty_param_bound)
        ) >>
        default: option!(preceded!(
            punct!("="),
            ty
        )) >>
        (TyParam {
            attrs: attrs,
            ident: id,
            bounds: bounds.unwrap_or_default(),
            colon_token: colon.map(|_| tokens::Colon::default()),
            eq_token: default.as_ref().map(|_| tokens::Eq::default()),
            default: default,
        })
    ));

    named!(pub ty_param_bound -> TyParamBound, alt!(
        preceded!(punct!("?"), poly_trait_ref) => {
            |poly| TyParamBound::Trait(poly, TraitBoundModifier::Maybe(tokens::Question::default()))
        }
        |
        lifetime => { TyParamBound::Region }
        |
        poly_trait_ref => {
            |poly| TyParamBound::Trait(poly, TraitBoundModifier::None)
        }
    ));

    named!(pub where_clause -> WhereClause, alt!(
        do_parse!(
            keyword!("where") >>
            predicates: terminated_list!(
                map!(punct!(","), |_| tokens::Comma::default()),
                where_predicate
            ) >>
            (WhereClause {
                predicates: predicates,
                where_token: Some(tokens::Where::default()),
            })
        )
        |
        epsilon!() => { |_| WhereClause::default() }
    ));

    named!(where_predicate -> WherePredicate, alt!(
        do_parse!(
            ident: lifetime >>
            colon: option!(punct!(":")) >>
            bounds: cond!(
                colon.is_some(),
                separated_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                lifetime)
            ) >>
            (WherePredicate::RegionPredicate(WhereRegionPredicate {
                lifetime: ident,
                bounds: bounds.unwrap_or_default(),
                colon_token: colon.map(|_| tokens::Colon::default()),
            }))
        )
        |
        do_parse!(
            bound_lifetimes: bound_lifetimes >>
            bounded_ty: ty >>
            punct!(":") >>
            bounds: separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                             ty_param_bound) >>
            (WherePredicate::BoundPredicate(WhereBoundPredicate {
                bound_lifetimes: bound_lifetimes,
                bounded_ty: bounded_ty,
                bounds: bounds,
                colon_token: tokens::Colon::default(),
            }))
        )
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Generics {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lt_token.to_tokens(tokens);
            self.lifetimes.to_tokens(tokens);
            self.ty_params.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    impl<'a> ToTokens for ImplGenerics<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.lt_token.to_tokens(tokens);
            self.0.lifetimes.to_tokens(tokens);
            for param in self.0.ty_params.iter() {
                 // Leave off the type parameter defaults
                let item = param.item();
                tokens.append_all(item.attrs.outer());
                item.ident.to_tokens(tokens);
                item.colon_token.to_tokens(tokens);
                item.bounds.to_tokens(tokens);
                param.delimiter().to_tokens(tokens);
            }
            self.0.gt_token.to_tokens(tokens);
        }
    }

    impl<'a> ToTokens for TyGenerics<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.lt_token.to_tokens(tokens);
            // Leave off the lifetime bounds and attributes
            for param in self.0.lifetimes.iter() {
                param.item().lifetime.to_tokens(tokens);
                param.delimiter().to_tokens(tokens);
            }
            // Leave off the type parameter defaults
            for param in self.0.ty_params.iter() {
                param.item().ident.to_tokens(tokens);
                param.delimiter().to_tokens(tokens);
            }
            self.0.gt_token.to_tokens(tokens);
        }
    }

    impl<'a> ToTokens for Turbofish<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let has_lifetimes = !self.0.lifetimes.is_empty();
            let has_ty_params = !self.0.ty_params.is_empty();
            if has_lifetimes || has_ty_params {
                tokens::Colon2::default().to_tokens(tokens);
                TyGenerics(self.0).to_tokens(tokens);
            }
        }
    }

    impl ToTokens for Lifetime {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
        }
    }

    impl ToTokens for BoundLifetimes {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.for_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);
            self.lifetimes.to_tokens(tokens);
            self.gt_token.to_tokens(tokens);
        }
    }

    impl ToTokens for LifetimeDef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.lifetime.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for TyParam {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.default.to_tokens(tokens);
        }
    }

    impl ToTokens for TyParamBound {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                TyParamBound::Region(ref lifetime) => lifetime.to_tokens(tokens),
                TyParamBound::Trait(ref trait_ref, ref modifier) => {
                    modifier.to_tokens(tokens);
                    trait_ref.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for TraitBoundModifier {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                TraitBoundModifier::None => {}
                TraitBoundModifier::Maybe(ref t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for WhereClause {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.where_token.to_tokens(tokens);
            self.predicates.to_tokens(tokens);
        }
    }

    impl ToTokens for WhereBoundPredicate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bound_lifetimes.to_tokens(tokens);
            self.bounded_ty.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for WhereRegionPredicate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lifetime.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for WhereEqPredicate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lhs_ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.rhs_ty.to_tokens(tokens);
        }
    }
}
