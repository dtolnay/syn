use crate::attr::Attribute;
use crate::expr::{Expr, Index, Member};
use crate::ident::Ident;
use crate::punctuated::{self, Punctuated};
use crate::restriction::{FieldMutability, Visibility};
use crate::token;
use crate::ty::Type;

ast_struct! {
    /// An enum variant.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct Variant {
        pub attrs: Vec<Attribute>,

        /// Name of the variant.
        pub ident: Ident,

        /// Content stored in the variant.
        pub fields: Fields,

        /// Explicit discriminant: `Variant = 1`
        pub discriminant: Option<(Token![=], Expr)>,
    }
}

ast_enum_of_structs! {
    /// Data stored within an enum variant or struct.
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub enum Fields {
        /// Named fields of a struct or struct variant such as `Point { x: f64,
        /// y: f64 }`.
        Named(FieldsNamed),

        /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
        Unnamed(FieldsUnnamed),

        /// Unit struct or unit variant such as `None`.
        Unit,
    }
}

ast_struct! {
    /// Named fields of a struct or struct variant such as `Point { x: f64,
    /// y: f64 }`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsNamed {
        pub brace_token: token::Brace,
        pub named: Punctuated<Field, Token![,]>,
    }
}

ast_struct! {
    /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsUnnamed {
        pub paren_token: token::Paren,
        pub unnamed: Punctuated<Field, Token![,]>,
    }
}

impl Fields {
    /// Get an iterator over the borrowed [`Field`] items in this object. This
    /// iterator can be used to iterate over a named or unnamed struct or
    /// variant's fields uniformly.
    pub fn iter(&self) -> punctuated::Iter<Field> {
        match self {
            Fields::Unit => crate::punctuated::empty_punctuated_iter(),
            Fields::Named(f) => f.named.iter(),
            Fields::Unnamed(f) => f.unnamed.iter(),
        }
    }

    /// Get an iterator over the mutably borrowed [`Field`] items in this
    /// object. This iterator can be used to iterate over a named or unnamed
    /// struct or variant's fields uniformly.
    pub fn iter_mut(&mut self) -> punctuated::IterMut<Field> {
        match self {
            Fields::Unit => crate::punctuated::empty_punctuated_iter_mut(),
            Fields::Named(f) => f.named.iter_mut(),
            Fields::Unnamed(f) => f.unnamed.iter_mut(),
        }
    }

    /// Returns the number of fields.
    pub fn len(&self) -> usize {
        match self {
            Fields::Unit => 0,
            Fields::Named(f) => f.named.len(),
            Fields::Unnamed(f) => f.unnamed.len(),
        }
    }

    /// Returns `true` if there are zero fields.
    pub fn is_empty(&self) -> bool {
        match self {
            Fields::Unit => true,
            Fields::Named(f) => f.named.is_empty(),
            Fields::Unnamed(f) => f.unnamed.is_empty(),
        }
    }

    return_impl_trait! {
        /// Get an iterator over the fields of a struct or variant as [`Member`]s.
        /// This iterator can be used to iterate over a named or unnamed struct or
        /// variant's fields uniformly.
        ///
        /// # Example
        ///
        /// The following is a simplistic [`Clone`] derive for structs. (A more
        /// complete implementation would additionally want to infer trait bounds on
        /// the generic type parameters.)
        ///
        /// ```
        /// # use quote::quote;
        /// #
        /// fn derive_clone(input: &syn::ItemStruct) -> proc_macro2::TokenStream {
        ///     let ident = &input.ident;
        ///     let members = input.fields.members();
        ///     let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
        ///     quote! {
        ///         impl #impl_generics Clone for #ident #ty_generics #where_clause {
        ///             fn clone(&self) -> Self {
        ///                 Self {
        ///                     #(#members: self.#members.clone()),*
        ///                 }
        ///             }
        ///         }
        ///     }
        /// }
        /// ```
        ///
        /// For structs with named fields, it produces an expression like `Self { a:
        /// self.a.clone() }`. For structs with unnamed fields, `Self { 0:
        /// self.0.clone() }`. And for unit structs, `Self {}`.
        pub fn members(&self) -> impl Iterator<Item = Member> + Clone + '_ [Members] {
            Members {
                fields: self.iter(),
                index: 0,
            }
        }
    }
}

impl IntoIterator for Fields {
    type Item = Field;
    type IntoIter = punctuated::IntoIter<Field>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Fields::Unit => Punctuated::<Field, ()>::new().into_iter(),
            Fields::Named(f) => f.named.into_iter(),
            Fields::Unnamed(f) => f.unnamed.into_iter(),
        }
    }
}

impl<'a> IntoIterator for &'a Fields {
    type Item = &'a Field;
    type IntoIter = punctuated::Iter<'a, Field>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Fields {
    type Item = &'a mut Field;
    type IntoIter = punctuated::IterMut<'a, Field>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

ast_struct! {
    /// A field of a struct or enum variant.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct Field {
        pub attrs: Vec<Attribute>,

        pub vis: Visibility,

        pub mutability: FieldMutability,

        /// Name of the field, if any.
        ///
        /// Fields of tuple structs have no names.
        pub ident: Option<Ident>,

        pub colon_token: Option<Token![:]>,

        pub ty: Type,
    }
}

pub struct Members<'a> {
    fields: punctuated::Iter<'a, Field>,
    index: u32,
}

impl<'a> Iterator for Members<'a> {
    type Item = Member;

    fn next(&mut self) -> Option<Self::Item> {
        let field = self.fields.next()?;
        let member = match &field.ident {
            Some(ident) => Member::Named(ident.clone()),
            None => {
                #[cfg(all(feature = "parsing", feature = "printing"))]
                let span = crate::spanned::Spanned::span(&field.ty);
                #[cfg(not(all(feature = "parsing", feature = "printing")))]
                let span = proc_macro2::Span::call_site();
                Member::Unnamed(Index {
                    index: self.index,
                    span,
                })
            }
        };
        self.index += 1;
        Some(member)
    }
}

impl<'a> Clone for Members<'a> {
    fn clone(&self) -> Self {
        Members {
            fields: self.fields.clone(),
            index: self.index,
        }
    }
}

ast_struct! {
    /// An enum variant.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct VariantWithDefault {
        pub attrs: Vec<Attribute>,

        /// Name of the variant.
        pub ident: Ident,

        /// Content stored in the variant.
        pub fields: FieldsWithDefault,

        /// Explicit discriminant: `Variant = 1`
        pub discriminant: Option<(Token![=], Expr)>,
    }
}

ast_enum_of_structs! {
    /// Data stored within an enum variant or struct.
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: crate::expr::Expr#syntax-tree-enums
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub enum FieldsWithDefault {
        /// Named fields of a struct or struct variant such as `Point { x: f64,
        /// y: f64 }`.
        Named(FieldsNamedWithDefault),

        /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
        Unnamed(FieldsUnnamedWithDefault),

        /// Unit struct or unit variant such as `None`.
        Unit,
    }
}

ast_struct! {
    /// Named fields of a struct or struct variant such as `Point { x: f64,
    /// y: f64 }`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsNamedWithDefault {
        pub brace_token: token::Brace,
        pub named: Punctuated<FieldWithDefault, Token![,]>,
    }
}

ast_struct! {
    /// Unnamed fields of a tuple struct or tuple variant such as `Some(T)`.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldsUnnamedWithDefault {
        pub paren_token: token::Paren,
        pub unnamed: Punctuated<FieldWithDefault, Token![,]>,
    }
}

impl FieldsWithDefault {
    /// Get an iterator over the borrowed [`Field`] items in this object. This
    /// iterator can be used to iterate over a named or unnamed struct or
    /// variant's fields uniformly.
    pub fn iter(&self) -> punctuated::Iter<FieldWithDefault> {
        match self {
            FieldsWithDefault::Unit => crate::punctuated::empty_punctuated_iter(),
            FieldsWithDefault::Named(f) => f.named.iter(),
            FieldsWithDefault::Unnamed(f) => f.unnamed.iter(),
        }
    }

    /// Get an iterator over the mutably borrowed [`Field`] items in this
    /// object. This iterator can be used to iterate over a named or unnamed
    /// struct or variant's fields uniformly.
    pub fn iter_mut(&mut self) -> punctuated::IterMut<FieldWithDefault> {
        match self {
            FieldsWithDefault::Unit => crate::punctuated::empty_punctuated_iter_mut(),
            FieldsWithDefault::Named(f) => f.named.iter_mut(),
            FieldsWithDefault::Unnamed(f) => f.unnamed.iter_mut(),
        }
    }

    /// Returns the number of fields.
    pub fn len(&self) -> usize {
        match self {
            FieldsWithDefault::Unit => 0,
            FieldsWithDefault::Named(f) => f.named.len(),
            FieldsWithDefault::Unnamed(f) => f.unnamed.len(),
        }
    }

    /// Returns `true` if there are zero fields.
    pub fn is_empty(&self) -> bool {
        match self {
            FieldsWithDefault::Unit => true,
            FieldsWithDefault::Named(f) => f.named.is_empty(),
            FieldsWithDefault::Unnamed(f) => f.unnamed.is_empty(),
        }
    }

    return_impl_trait! {
        /// Get an iterator over the fields of a struct or variant as [`Member`]s.
        /// This iterator can be used to iterate over a named or unnamed struct or
        /// variant's fields uniformly.
        ///
        /// # Example
        ///
        /// The following is a simplistic [`Clone`] derive for structs. (A more
        /// complete implementation would additionally want to infer trait bounds on
        /// the generic type parameters.)
        ///
        /// ```
        /// # use quote::quote;
        /// #
        /// fn derive_clone(input: &syn::ItemStruct) -> proc_macro2::TokenStream {
        ///     let ident = &input.ident;
        ///     let members = input.fields.members();
        ///     let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
        ///     quote! {
        ///         impl #impl_generics Clone for #ident #ty_generics #where_clause {
        ///             fn clone(&self) -> Self {
        ///                 Self {
        ///                     #(#members: self.#members.clone()),*
        ///                 }
        ///             }
        ///         }
        ///     }
        /// }
        /// ```
        ///
        /// For structs with named fields, it produces an expression like `Self { a:
        /// self.a.clone() }`. For structs with unnamed fields, `Self { 0:
        /// self.0.clone() }`. And for unit structs, `Self {}`.
        pub fn members(&self) -> impl Iterator<Item = Member> + Clone + '_ [MembersWithDefault] {
            MembersWithDefault {
                fields: self.iter(),
                index: 0,
            }
        }
    }
}

impl IntoIterator for FieldsWithDefault {
    type Item = FieldWithDefault;
    type IntoIter = punctuated::IntoIter<FieldWithDefault>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            FieldsWithDefault::Unit => Punctuated::<FieldWithDefault, ()>::new().into_iter(),
            FieldsWithDefault::Named(f) => f.named.into_iter(),
            FieldsWithDefault::Unnamed(f) => f.unnamed.into_iter(),
        }
    }
}

impl<'a> IntoIterator for &'a FieldsWithDefault {
    type Item = &'a FieldWithDefault;
    type IntoIter = punctuated::Iter<'a, FieldWithDefault>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut FieldsWithDefault {
    type Item = &'a mut FieldWithDefault;
    type IntoIter = punctuated::IterMut<'a, FieldWithDefault>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

ast_struct! {
    /// A field of a struct or enum variant, possibly with a provided (const) default.
    #[cfg_attr(docsrs, doc(cfg(any(feature = "full", feature = "derive"))))]
    pub struct FieldWithDefault {
        pub attrs: Vec<Attribute>,

        pub vis: Visibility,

        pub mutability: FieldMutability,

        /// Name of the field, if any.
        ///
        /// Fields of tuple structs have no names.
        pub ident: Option<Ident>,

        pub colon_token: Option<Token![:]>,

        pub ty: Type,

        /// Default value: `field_name: i32 = 1`
        ///
        /// `#![feature(default_field_values)]`
        pub default: Option<(Token![=], Expr)>,
    }
}

pub struct MembersWithDefault<'a> {
    fields: punctuated::Iter<'a, FieldWithDefault>,
    index: u32,
}

impl<'a> Iterator for MembersWithDefault<'a> {
    type Item = Member;

    fn next(&mut self) -> Option<Self::Item> {
        let field = self.fields.next()?;
        let member = match &field.ident {
            Some(ident) => Member::Named(ident.clone()),
            None => {
                #[cfg(all(feature = "parsing", feature = "printing"))]
                let span = crate::spanned::Spanned::span(&field.ty);
                #[cfg(not(all(feature = "parsing", feature = "printing")))]
                let span = proc_macro2::Span::call_site();
                Member::Unnamed(Index {
                    index: self.index,
                    span,
                })
            }
        };
        self.index += 1;
        Some(member)
    }
}

impl<'a> Clone for MembersWithDefault<'a> {
    fn clone(&self) -> Self {
        MembersWithDefault {
            fields: self.fields.clone(),
            index: self.index,
        }
    }
}

#[cfg(feature = "parsing")]
pub(crate) mod parsing {
    use crate::attr::Attribute;
    use crate::data::{Field, Fields, FieldsNamed, FieldsUnnamed, Variant};
    use crate::error::Result;
    use crate::expr::Expr;
    use crate::ext::IdentExt as _;
    use crate::ident::Ident;
    #[cfg(not(feature = "full"))]
    use crate::parse::discouraged::Speculative as _;
    use crate::parse::{Parse, ParseStream};
    use crate::restriction::{FieldMutability, Visibility};
    #[cfg(not(feature = "full"))]
    use crate::scan_expr::scan_expr;
    use crate::ty::Type;
    use crate::verbatim;
    use crate::{
        token, FieldWithDefault, FieldsNamedWithDefault, FieldsUnnamedWithDefault,
        FieldsWithDefault, VariantWithDefault,
    };

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for Variant {
        fn parse(input: ParseStream) -> Result<Self> {
            let (attrs, ident, fields, discriminant) = parse_variants(input)?;
            Ok(Variant {
                attrs,
                ident,
                fields,
                discriminant,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for VariantWithDefault {
        fn parse(input: ParseStream) -> Result<Self> {
            let (attrs, ident, fields, discriminant) = parse_variants(input)?;
            Ok(VariantWithDefault {
                attrs,
                ident,
                fields,
                discriminant,
            })
        }
    }

    // Internal function, only used directly above.
    #[allow(clippy::type_complexity)]
    fn parse_variants<F: FieldsStruct>(
        input: &crate::parse::ParseBuffer<'_>,
    ) -> Result<(Vec<Attribute>, Ident, F, Option<(Token![=], Expr)>)> {
        let attrs = input.call(Attribute::parse_outer)?;
        let _visibility: Visibility = input.parse()?;
        let ident: Ident = input.parse()?;
        let fields = if input.peek(token::Brace) {
            F::named(input.parse()?)
        } else if input.peek(token::Paren) {
            F::unnamed(input.parse()?)
        } else {
            F::unit()
        };
        let discriminant = if input.peek(Token![=]) {
            let eq_token: Token![=] = input.parse()?;
            #[cfg(feature = "full")]
            let discriminant: Expr = input.parse()?;
            #[cfg(not(feature = "full"))]
            let discriminant = {
                let begin = input.fork();
                let ahead = input.fork();
                let mut discriminant: Result<Expr> = ahead.parse();
                if discriminant.is_ok() {
                    input.advance_to(&ahead);
                } else if scan_expr(input).is_ok() {
                    discriminant = Ok(Expr::Verbatim(verbatim::between(&begin, input)));
                }
                discriminant?
            };
            Some((eq_token, discriminant))
        } else {
            None
        };
        Ok((attrs, ident, fields, discriminant))
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsNamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsNamed {
                brace_token: braced!(content in input),
                named: content.parse_terminated(Field::parse_named, Token![,])?,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsUnnamed {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsUnnamed {
                paren_token: parenthesized!(content in input),
                unnamed: content.parse_terminated(Field::parse_unnamed, Token![,])?,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsNamedWithDefault {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsNamedWithDefault {
                brace_token: braced!(content in input),
                named: content.parse_terminated(FieldWithDefault::parse_named, Token![,])?,
            })
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
    impl Parse for FieldsUnnamedWithDefault {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(FieldsUnnamedWithDefault {
                paren_token: parenthesized!(content in input),
                unnamed: content.parse_terminated(FieldWithDefault::parse_unnamed, Token![,])?,
            })
        }
    }

    impl Field {
        /// Parses a named (braced struct) field.
        #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
        pub fn parse_named(input: ParseStream) -> Result<Self> {
            let (res, default) = Self::parse_named_with_default(input)?;
            if let Some((_eq, _)) = default {
                // We choose not to emit an error in this case, because ignoring it brings implicit support
                // for ignoring default field values to ecosystem derive macros.
                // The counterpoint is that it will be confusing for non-derive macros (or external
                // parsing), although it's much rarer to find those on structs/enums.
                // (This confusion would be because the corresponding `ToTokens` impl will just silently
                // exclude the default field value)
            }
            Ok(res)
        }

        /// Parses a named (braced struct) field, optionally returning the default value.
        #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
        fn parse_named_with_default(
            input: ParseStream,
        ) -> Result<(Self, Option<(Token![=], Expr)>)> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis: Visibility = input.parse()?;

            let unnamed_field = cfg!(feature = "full") && input.peek(Token![_]);
            let ident = if unnamed_field {
                input.call(Ident::parse_any)
            } else {
                input.parse()
            }?;

            let colon_token: Token![:] = input.parse()?;

            let ty: Type = if unnamed_field
                && (input.peek(Token![struct])
                    || input.peek(Token![union]) && input.peek2(token::Brace))
            {
                let begin = input.fork();
                input.call(Ident::parse_any)?;
                input.parse::<FieldsNamed>()?;
                Type::Verbatim(verbatim::between(&begin, input))
            } else {
                input.parse()?
            };

            let mut default: Option<(Token![=], Expr)> = None;
            if input.peek(Token![=]) {
                let eq_token: Token![=] = input.parse()?;
                default = Some((eq_token, input.parse()?));
            }

            Ok((
                Field {
                    attrs,
                    vis,
                    mutability: FieldMutability::None,
                    ident: Some(ident),
                    colon_token: Some(colon_token),
                    ty,
                },
                default,
            ))
        }

        /// Parses an unnamed (tuple struct) field.
        #[cfg_attr(docsrs, doc(cfg(feature = "parsing")))]
        pub fn parse_unnamed(input: ParseStream) -> Result<Self> {
            Ok(Field {
                attrs: input.call(Attribute::parse_outer)?,
                vis: input.parse()?,
                mutability: FieldMutability::None,
                ident: None,
                colon_token: None,
                ty: input.parse()?,
            })
        }
    }
    impl FieldWithDefault {
        pub fn parse_named(input: ParseStream) -> Result<Self> {
            let (
                Field {
                    attrs,
                    vis,
                    mutability,
                    ident,
                    colon_token,
                    ty,
                },
                default,
            ) = Field::parse_named_with_default(input)?;
            Ok(Self {
                attrs,
                vis,
                mutability,
                ident,
                colon_token,
                ty,
                default,
            })
        }

        /// Parses an unnamed (tuple struct) field.
        ///
        /// There is no need for default field values in.
        pub fn parse_unnamed(input: ParseStream) -> Result<Self> {
            let Field {
                attrs,
                vis,
                mutability,
                ident,
                colon_token,
                ty,
            } = Field::parse_unnamed(input)?;
            Ok(Self {
                attrs,
                vis,
                mutability,
                ident,
                colon_token,
                ty,
                default: None,
            })
        }
    }

    /// An internal trait to allow abstracting minimally over [`Fields`] and [`FieldsUnnamed`] in parsing.
    pub(crate) trait FieldsStruct {
        type NamedTy: Parse;
        type UnnamedTy: Parse;

        fn named(named: Self::NamedTy) -> Self;
        fn unnamed(unnamed: Self::UnnamedTy) -> Self;
        fn unit() -> Self;
    }

    impl FieldsStruct for Fields {
        type NamedTy = FieldsNamed;
        type UnnamedTy = FieldsUnnamed;
        fn named(named: Self::NamedTy) -> Self {
            Self::Named(named)
        }
        fn unnamed(unnamed: Self::UnnamedTy) -> Self {
            Self::Unnamed(unnamed)
        }
        fn unit() -> Self {
            Self::Unit
        }
    }

    impl FieldsStruct for FieldsWithDefault {
        type NamedTy = FieldsNamedWithDefault;
        type UnnamedTy = FieldsUnnamedWithDefault;
        fn named(named: Self::NamedTy) -> Self {
            Self::Named(named)
        }
        fn unnamed(unnamed: Self::UnnamedTy) -> Self {
            Self::Unnamed(unnamed)
        }
        fn unit() -> Self {
            Self::Unit
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use crate::data::{Field, FieldsNamed, FieldsUnnamed, Variant};
    use crate::print::TokensOrDefault;
    use crate::{
        FieldWithDefault, FieldsNamedWithDefault, FieldsUnnamedWithDefault, VariantWithDefault,
    };
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt as _};

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Variant {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.ident.to_tokens(tokens);
            self.fields.to_tokens(tokens);
            if let Some((eq_token, disc)) = &self.discriminant {
                eq_token.to_tokens(tokens);
                disc.to_tokens(tokens);
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsNamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                self.named.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsUnnamed {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.unnamed.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for Field {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            if let Some(ident) = &self.ident {
                ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);
        }
    }
    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for VariantWithDefault {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.ident.to_tokens(tokens);
            self.fields.to_tokens(tokens);
            if let Some((eq_token, disc)) = &self.discriminant {
                eq_token.to_tokens(tokens);
                disc.to_tokens(tokens);
            }
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsNamedWithDefault {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.brace_token.surround(tokens, |tokens| {
                self.named.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldsUnnamedWithDefault {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.unnamed.to_tokens(tokens);
            });
        }
    }

    #[cfg_attr(docsrs, doc(cfg(feature = "printing")))]
    impl ToTokens for FieldWithDefault {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(&self.attrs);
            self.vis.to_tokens(tokens);
            if let Some(ident) = &self.ident {
                ident.to_tokens(tokens);
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
            }
            self.ty.to_tokens(tokens);
            if let Some((eq, default)) = &self.default {
                eq.to_tokens(tokens);
                default.to_tokens(tokens);
            }
        }
    }
}
