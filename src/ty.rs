// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use punctuated::Punctuated;
use super::*;
use proc_macro2::TokenStream;
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "extra-traits")]
use tt::TokenStreamHelper;

ast_enum_of_structs! {
    /// The different kinds of types recognized by the compiler
    pub enum Type {
        /// A variable-length array (`[T]`)
        pub Slice(TypeSlice {
            pub bracket_token: token::Bracket,
            pub elem: Box<Type>,
        }),
        /// A fixed length array (`[T; n]`)
        pub Array(TypeArray {
            pub bracket_token: token::Bracket,
            pub elem: Box<Type>,
            pub semi_token: Token![;],
            pub len: Expr,
        }),
        /// A raw pointer (`*const T` or `*mut T`)
        pub Ptr(TypePtr {
            pub star_token: Token![*],
            pub const_token: Option<Token![const]>,
            pub mutability: Option<Token![mut]>,
            pub elem: Box<Type>,
        }),
        /// A reference (`&'a T` or `&'a mut T`)
        pub Reference(TypeReference {
            pub and_token: Token![&],
            pub lifetime: Option<Lifetime>,
            pub mutability: Option<Token![mut]>,
            pub elem: Box<Type>,
        }),
        /// A bare function (e.g. `fn(usize) -> bool`)
        pub BareFn(TypeBareFn {
            pub unsafety: Option<Token![unsafe]>,
            pub abi: Option<Abi>,
            pub fn_token: Token![fn],
            pub lifetimes: Option<BoundLifetimes>,
            pub paren_token: token::Paren,
            pub inputs: Punctuated<BareFnArg, Token![,]>,
            pub variadic: Option<Token![...]>,
            pub output: ReturnType,
        }),
        /// The never type (`!`)
        pub Never(TypeNever {
            pub bang_token: Token![!],
        }),
        /// A tuple (`(A, B, C, D, ...)`)
        pub Tuple(TypeTuple {
            pub paren_token: token::Paren,
            pub elems: Punctuated<Type, Token![,]>,
        }),
        /// A path (`module::module::...::Type`), optionally
        /// "qualified", e.g. `<Vec<T> as SomeTrait>::SomeType`.
        ///
        /// Type arguments are stored in the Path itself
        pub Path(TypePath {
            pub qself: Option<QSelf>,
            pub path: Path,
        }),
        /// A trait object type `Bound1 + Bound2 + Bound3`
        /// where `Bound` is a trait or a lifetime.
        pub TraitObject(TypeTraitObject {
            pub dyn_token: Option<Token![dyn]>,
            pub bounds: Punctuated<TypeParamBound, Token![+]>,
        }),
        /// An `impl Bound1 + Bound2 + Bound3` type
        /// where `Bound` is a trait or a lifetime.
        pub ImplTrait(TypeImplTrait {
            pub impl_token: Token![impl],
            pub bounds: Punctuated<TypeParamBound, Token![+]>,
        }),
        /// No-op; kept solely so that we can pretty-print faithfully
        pub Paren(TypeParen {
            pub paren_token: token::Paren,
            pub elem: Box<Type>,
        }),
        /// No-op: kept solely so that we can pretty-print faithfully
        pub Group(TypeGroup {
            pub group_token: token::Group,
            pub elem: Box<Type>,
        }),
        /// TypeKind::Infer means the type should be inferred instead of it having been
        /// specified. This can appear anywhere in a type.
        pub Infer(TypeInfer {
            pub underscore_token: Token![_],
        }),
        /// A macro in the type position.
        pub Macro(TypeMacro {
            pub mac: Macro,
        }),
        pub Verbatim(TypeVerbatim #manual_extra_traits {
            pub tts: TokenStream,
        }),
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for TypeVerbatim {}

#[cfg(feature = "extra-traits")]
impl PartialEq for TypeVerbatim {
    fn eq(&self, other: &Self) -> bool {
        TokenStreamHelper(&self.tts) == TokenStreamHelper(&other.tts)
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for TypeVerbatim {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        TokenStreamHelper(&self.tts).hash(state);
    }
}

ast_struct! {
    pub struct Abi {
        pub extern_token: Token![extern],
        pub name: Option<Lit>,
    }
}

ast_struct! {
    /// An argument in a function type.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub struct BareFnArg {
        pub name: Option<(BareFnArgName, Token![:])>,
        pub ty: Type,
    }
}

ast_enum! {
    /// Names of arguments in the `BareFnArg` structure
    pub enum BareFnArgName {
        /// Argument with the provided name
        Named(Ident),
        /// Argument matched with `_`
        Wild(Token![_]),
    }
}

ast_enum! {
    pub enum ReturnType {
        /// Return type is not specified.
        ///
        /// Functions default to `()` and
        /// closures default to inference. Span points to where return
        /// type would be inserted.
        Default,
        /// Everything else
        Type(Token![->], Box<Type>),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::Synom;
    use path::parsing::qpath;

    impl Synom for Type {
        named!(parse -> Self, call!(ambig_ty, true));

        fn description() -> Option<&'static str> {
            Some("type")
        }
    }

    impl Type {
        /// In some positions, types may not contain the `+` character, to
        /// disambiguate them. For example in the expression `1 as T`, T may not
        /// contain a `+` character.
        ///
        /// This parser does not allow a `+`, while the default parser does.
        named!(pub without_plus -> Self, call!(ambig_ty, false));
    }

    named!(ambig_ty(allow_plus: bool) -> Type, alt!(
        syn!(TypeGroup) => { Type::Group }
        |
        // must be before TypeTuple
        call!(TypeParen::parse, allow_plus) => { Type::Paren }
        |
        // must be before TypePath
        syn!(TypeMacro) => { Type::Macro }
        |
        // must be before TypeTraitObject
        call!(TypePath::parse, allow_plus) => { Type::Path }
        |
        // Don't try parsing more than one trait bound if we aren't allowing it.
        // must be before TypeTuple
        call!(TypeTraitObject::parse, allow_plus) => { Type::TraitObject }
        |
        syn!(TypeSlice) => { Type::Slice }
        |
        syn!(TypeArray) => { Type::Array }
        |
        syn!(TypePtr) => { Type::Ptr }
        |
        syn!(TypeReference) => { Type::Reference }
        |
        syn!(TypeBareFn) => { Type::BareFn }
        |
        syn!(TypeNever) => { Type::Never }
        |
        syn!(TypeTuple) => { Type::Tuple }
        |
        syn!(TypeImplTrait) => { Type::ImplTrait }
        |
        syn!(TypeInfer) => { Type::Infer }
    ));

    impl Synom for TypeSlice {
        named!(parse -> Self, map!(
            brackets!(syn!(Type)),
            |(b, ty)| TypeSlice {
                elem: Box::new(ty),
                bracket_token: b,
            }
        ));

        fn description() -> Option<&'static str> {
            Some("slice type")
        }
    }

    impl Synom for TypeArray {
        named!(parse -> Self, map!(
            brackets!(do_parse!(
                elem: syn!(Type) >>
                    semi: punct!(;) >>
                    len: syn!(Expr) >>
                    (elem, semi, len)
            )),
            |(brackets, (elem, semi, len))| {
                TypeArray {
                    elem: Box::new(elem),
                    len: len,
                    bracket_token: brackets,
                    semi_token: semi,
                }
            }
        ));

        fn description() -> Option<&'static str> {
            Some("array type")
        }
    }

    impl Synom for TypePtr {
        named!(parse -> Self, do_parse!(
            star: punct!(*) >>
            mutability: alt!(
                keyword!(const) => { |c| (None, Some(c)) }
                |
                keyword!(mut) => { |m| (Some(m), None) }
            ) >>
            target: call!(Type::without_plus) >>
            (TypePtr {
                const_token: mutability.1,
                star_token: star,
                mutability: mutability.0,
                elem: Box::new(target),
            })
        ));

        fn description() -> Option<&'static str> {
            Some("raw pointer type")
        }
    }

    impl Synom for TypeReference {
        named!(parse -> Self, do_parse!(
            amp: punct!(&) >>
            life: option!(syn!(Lifetime)) >>
            mutability: option!(keyword!(mut)) >>
            // & binds tighter than +, so we don't allow + here.
            target: call!(Type::without_plus) >>
            (TypeReference {
                lifetime: life,
                mutability: mutability,
                elem: Box::new(target),
                and_token: amp,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("reference type")
        }
    }

    impl Synom for TypeBareFn {
        named!(parse -> Self, do_parse!(
            lifetimes: option!(syn!(BoundLifetimes)) >>
            unsafety: option!(keyword!(unsafe)) >>
            abi: option!(syn!(Abi)) >>
            fn_: keyword!(fn) >>
            parens: parens!(do_parse!(
                inputs: call!(Punctuated::parse_terminated) >>
                variadic: option!(cond_reduce!(inputs.empty_or_trailing(), punct!(...))) >>
                (inputs, variadic)
            )) >>
            output: syn!(ReturnType) >>
            (TypeBareFn {
                unsafety: unsafety,
                abi: abi,
                lifetimes: lifetimes,
                output: output,
                variadic: (parens.1).1,
                fn_token: fn_,
                paren_token: parens.0,
                inputs: (parens.1).0,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("`fn` type")
        }
    }

    impl Synom for TypeNever {
        named!(parse -> Self, map!(
            punct!(!),
            |b| TypeNever { bang_token: b }
        ));

        fn description() -> Option<&'static str> {
            Some("never type: `!`")
        }
    }

    impl Synom for TypeInfer {
        named!(parse -> Self, map!(
            punct!(_),
            |u| TypeInfer { underscore_token: u }
        ));

        fn description() -> Option<&'static str> {
            Some("inferred type: `_`")
        }
    }

    impl Synom for TypeTuple {
        named!(parse -> Self, do_parse!(
            data: parens!(Punctuated::parse_terminated) >>
            (TypeTuple {
                paren_token: data.0,
                elems: data.1,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("tuple type")
        }
    }

    impl Synom for TypeMacro {
        named!(parse -> Self, map!(syn!(Macro), |mac| TypeMacro { mac: mac }));
    }

    impl Synom for TypePath {
        named!(parse -> Self, call!(Self::parse, false));
    }

    impl TypePath {
        named!(parse(allow_plus: bool) -> Self, do_parse!(
            qpath: qpath >>
            parenthesized: option!(cond_reduce!(
                qpath.1.segments.last().unwrap().value().arguments.is_empty(),
                syn!(ParenthesizedGenericArguments)
            )) >>
            cond!(allow_plus, not!(punct!(+))) >>
            ({
                let (qself, mut path) = qpath;
                if let Some(parenthesized) = parenthesized {
                    let parenthesized = PathArguments::Parenthesized(parenthesized);
                    path.segments.last_mut().unwrap().value_mut().arguments = parenthesized;
                }
                TypePath { qself: qself, path: path }
            })
        ));
    }

    impl Synom for ReturnType {
        named!(parse -> Self, alt!(
            do_parse!(
                arrow: punct!(->) >>
                ty: syn!(Type) >>
                (ReturnType::Type(arrow, Box::new(ty)))
            )
            |
            epsilon!() => { |_| ReturnType::Default }
        ));

        fn description() -> Option<&'static str> {
            Some("return type")
        }
    }

    impl Synom for TypeTraitObject {
        named!(parse -> Self, call!(Self::parse, true));

        fn description() -> Option<&'static str> {
            Some("trait object type")
        }
    }

    impl TypeTraitObject {
        named!(pub without_plus -> Self, call!(Self::parse, false));

        // Only allow multiple trait references if allow_plus is true.
        named!(parse(allow_plus: bool) -> Self, do_parse!(
            dyn_token: option!(keyword!(dyn)) >>
            bounds: alt!(
                cond_reduce!(allow_plus, Punctuated::parse_terminated_nonempty)
                |
                syn!(TypeParamBound) => {|x| {
                    let mut bounds = Punctuated::new();
                    bounds.push_value(x);
                    bounds
                }}
            ) >>
            (TypeTraitObject {
                dyn_token: dyn_token,
                bounds: bounds,
            })
        ));
    }

    impl Synom for TypeImplTrait {
        named!(parse -> Self, do_parse!(
            impl_: keyword!(impl) >>
            // NOTE: rust-lang/rust#34511 includes discussion about whether or
            // not + should be allowed in ImplTrait directly without ().
            elem: call!(Punctuated::parse_terminated_nonempty) >>
            (TypeImplTrait {
                impl_token: impl_,
                bounds: elem,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("`impl Trait` type")
        }
    }

    impl Synom for TypeGroup {
        named!(parse -> Self, do_parse!(
            data: grouped!(syn!(Type)) >>
            (TypeGroup {
                group_token: data.0,
                elem: Box::new(data.1),
            })
        ));
    }

    impl Synom for TypeParen {
        named!(parse -> Self, call!(Self::parse, false));
    }

    impl TypeParen {
        named!(parse(allow_plus: bool) -> Self, do_parse!(
            data: parens!(syn!(Type)) >>
            cond!(allow_plus, not!(punct!(+))) >>
            (TypeParen {
                paren_token: data.0,
                elem: Box::new(data.1),
            })
        ));
    }

    impl Synom for BareFnArg {
        named!(parse -> Self, do_parse!(
            name: option!(do_parse!(
                name: syn!(BareFnArgName) >>
                not!(punct!(::)) >>
                colon: punct!(:) >>
                (name, colon)
            )) >>
            ty: syn!(Type) >>
            (BareFnArg {
                name: name,
                ty: ty,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("function type argument")
        }
    }

    impl Synom for BareFnArgName {
        named!(parse -> Self, alt!(
            map!(syn!(Ident), BareFnArgName::Named)
            |
            map!(punct!(_), BareFnArgName::Wild)
        ));

        fn description() -> Option<&'static str> {
            Some("function argument name")
        }
    }

    impl Synom for Abi {
        named!(parse -> Self, do_parse!(
            extern_: keyword!(extern) >>
            // TODO: this parses all literals, not just strings
            name: option!(syn!(Lit)) >>
            (Abi {
                extern_token: extern_,
                name: name,
            })
        ));

        fn description() -> Option<&'static str> {
            Some("ABI qualifier")
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{ToTokens, Tokens};

    impl ToTokens for TypeSlice {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.elem.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TypeArray {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bracket_token.surround(tokens, |tokens| {
                self.elem.to_tokens(tokens);
                self.semi_token.to_tokens(tokens);
                self.len.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TypePtr {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.star_token.to_tokens(tokens);
            match self.mutability {
                Some(ref tok) => tok.to_tokens(tokens),
                None => {
                    TokensOrDefault(&self.const_token).to_tokens(tokens);
                }
            }
            self.elem.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeReference {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.lifetime.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.elem.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeBareFn {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.lifetimes.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.abi.to_tokens(tokens);
            self.fn_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
                if let Some(ref variadic) = self.variadic {
                    if !self.inputs.empty_or_trailing() {
                        let span = variadic.0[0];
                        <Token![,]>::new(span).to_tokens(tokens);
                    }
                    variadic.to_tokens(tokens);
                }
            });
            self.output.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeNever {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.bang_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeTuple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for TypePath {
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

            // XXX: Gross.
            let pos = if qself.position > 0 && qself.position >= self.1.segments.len() {
                self.1.segments.len() - 1
            } else {
                qself.position
            };
            let mut segments = self.1.segments.pairs();
            if pos > 0 {
                TokensOrDefault(&qself.as_token).to_tokens(tokens);
                self.1.leading_colon.to_tokens(tokens);
                for (i, segment) in segments.by_ref().take(pos).enumerate() {
                    if i + 1 == pos {
                        segment.value().to_tokens(tokens);
                        qself.gt_token.to_tokens(tokens);
                        segment.punct().to_tokens(tokens);
                    } else {
                        segment.to_tokens(tokens);
                    }
                }
            } else {
                qself.gt_token.to_tokens(tokens);
                self.1.leading_colon.to_tokens(tokens);
            }
            for segment in segments {
                segment.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TypeTraitObject {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.dyn_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeImplTrait {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.impl_token.to_tokens(tokens);
            self.bounds.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeGroup {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.group_token.surround(tokens, |tokens| {
                self.elem.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TypeParen {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.paren_token.surround(tokens, |tokens| {
                self.elem.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TypeInfer {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.underscore_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeMacro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.mac.to_tokens(tokens);
        }
    }

    impl ToTokens for TypeVerbatim {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.tts.to_tokens(tokens);
        }
    }

    impl ToTokens for ReturnType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                ReturnType::Default => {}
                ReturnType::Type(ref arrow, ref ty) => {
                    arrow.to_tokens(tokens);
                    ty.to_tokens(tokens);
                }
            }
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

    impl ToTokens for BareFnArgName {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                BareFnArgName::Named(ref t) => t.to_tokens(tokens),
                BareFnArgName::Wild(ref t) => t.to_tokens(tokens),
            }
        }
    }

    impl ToTokens for Abi {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.extern_token.to_tokens(tokens);
            self.name.to_tokens(tokens);
        }
    }
}
