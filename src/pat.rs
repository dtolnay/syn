use super::*;
use crate::punctuated::Punctuated;
#[cfg(feature = "extra-traits")]
use crate::tt::TokenStreamHelper;
use proc_macro2::TokenStream;
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

ast_enum_of_structs! {
    /// A pattern in a local binding, function signature, match expression, or
    /// various other places.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: enum.Expr.html#syntax-tree-enums
    //
    // TODO: change syntax-tree-enum link to an intra rustdoc link, currently
    // blocked on https://github.com/rust-lang/rust/issues/62833
    pub enum Pat #manual_extra_traits {
        /// A box pattern: `box v`.
        Box(PatBox),

        /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
        Ident(PatIdent),

        /// A literal pattern: `0`.
        ///
        /// This holds an `Expr` rather than a `Lit` because negative numbers
        /// are represented as an `Expr::Unary`.
        Lit(PatLit),

        /// A macro in pattern position.
        Macro(PatMacro),

        /// A pattern that matches any one of a set of cases.
        Or(PatOr),

        /// A path pattern like `Color::Red`, optionally qualified with a
        /// self-type.
        ///
        /// Unqualified path patterns can legally refer to variants, structs,
        /// constants or associated constants. Qualified path patterns like
        /// `<A>::B::C` and `<A as Trait>::B::C` can only legally refer to
        /// associated constants.
        Path(PatPath),

        /// A range pattern: `1..=2`.
        Range(PatRange),

        /// A reference pattern: `&mut var`.
        Reference(PatReference),

        /// The dots in a tuple or slice pattern: `[0, 1, ..]`
        Rest(PatRest),

        /// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
        Slice(PatSlice),

        /// A struct or struct variant pattern: `Variant { x, y, .. }`.
        Struct(PatStruct),

        /// A tuple pattern: `(a, b)`.
        Tuple(PatTuple),

        /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
        TupleStruct(PatTupleStruct),

        /// A type ascription pattern: `foo: f64`.
        Type(PatType),

        /// Tokens in pattern position not interpreted by Syn.
        Verbatim(TokenStream),

        /// A pattern that matches any value: `_`.
        Wild(PatWild),

        #[doc(hidden)]
        __Nonexhaustive,
    }
}

ast_struct! {
    /// A box pattern: `box v`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatBox {
        pub attrs: Vec<Attribute>,
        pub box_token: Token![box],
        pub pat: Box<Pat>,
    }
}

ast_struct! {
    /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatIdent {
        pub attrs: Vec<Attribute>,
        pub by_ref: Option<Token![ref]>,
        pub mutability: Option<Token![mut]>,
        pub ident: Ident,
        pub subpat: Option<(Token![@], Box<Pat>)>,
    }
}

ast_struct! {
    /// A literal pattern: `0`.
    ///
    /// This holds an `Expr` rather than a `Lit` because negative numbers
    /// are represented as an `Expr::Unary`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatLit {
        pub attrs: Vec<Attribute>,
        pub expr: Box<Expr>,
    }
}

ast_struct! {
    /// A macro in pattern position.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatMacro {
        pub attrs: Vec<Attribute>,
        pub mac: Macro,
    }
}

ast_struct! {
    /// A pattern that matches any one of a set of cases.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatOr {
        pub attrs: Vec<Attribute>,
        pub leading_vert: Option<Token![|]>,
        pub cases: Punctuated<Pat, Token![|]>,
    }
}

ast_struct! {
    /// A path pattern like `Color::Red`, optionally qualified with a
    /// self-type.
    ///
    /// Unqualified path patterns can legally refer to variants, structs,
    /// constants or associated constants. Qualified path patterns like
    /// `<A>::B::C` and `<A as Trait>::B::C` can only legally refer to
    /// associated constants.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatPath {
        pub attrs: Vec<Attribute>,
        pub qself: Option<QSelf>,
        pub path: Path,
    }
}

ast_struct! {
    /// A range pattern: `1..=2`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatRange {
        pub attrs: Vec<Attribute>,
        pub lo: Box<Expr>,
        pub limits: RangeLimits,
        pub hi: Box<Expr>,
    }
}

ast_struct! {
    /// A reference pattern: `&mut var`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatReference {
        pub attrs: Vec<Attribute>,
        pub and_token: Token![&],
        pub mutability: Option<Token![mut]>,
        pub pat: Box<Pat>,
    }
}

ast_struct! {
    /// The dots in a tuple or slice pattern: `[0, 1, ..]`
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatRest {
        pub attrs: Vec<Attribute>,
        pub dot2_token: Token![..],
    }
}

ast_struct! {
    /// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatSlice {
        pub attrs: Vec<Attribute>,
        pub bracket_token: token::Bracket,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    /// A struct or struct variant pattern: `Variant { x, y, .. }`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatStruct {
        pub attrs: Vec<Attribute>,
        pub path: Path,
        pub brace_token: token::Brace,
        pub fields: Punctuated<FieldPat, Token![,]>,
        pub dot2_token: Option<Token![..]>,
    }
}

ast_struct! {
    /// A tuple pattern: `(a, b)`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatTuple {
        pub attrs: Vec<Attribute>,
        pub paren_token: token::Paren,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatTupleStruct {
        pub attrs: Vec<Attribute>,
        pub path: Path,
        pub pat: PatTuple,
    }
}

ast_struct! {
    /// A type ascription pattern: `foo: f64`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatType {
        pub attrs: Vec<Attribute>,
        pub pat: Box<Pat>,
        pub colon_token: Token![:],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    /// A pattern that matches any value: `_`.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct PatWild {
        pub attrs: Vec<Attribute>,
        pub underscore_token: Token![_],
    }
}

ast_struct! {
    /// A single field in a struct pattern.
    ///
    /// Patterns like the fields of Foo `{ x, ref y, ref mut z }` are treated
    /// the same as `x: x, y: ref y, z: ref mut z` but there is no colon token.
    ///
    /// *This type is available if Syn is built with the `"full"` feature.*
    pub struct FieldPat {
        pub attrs: Vec<Attribute>,
        pub member: Member,
        pub colon_token: Option<Token![:]>,
        pub pat: Box<Pat>,
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Pat {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Pat {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pat::Box(this), Pat::Box(other)) => this == other,
            (Pat::Ident(this), Pat::Ident(other)) => this == other,
            (Pat::Lit(this), Pat::Lit(other)) => this == other,
            (Pat::Macro(this), Pat::Macro(other)) => this == other,
            (Pat::Or(this), Pat::Or(other)) => this == other,
            (Pat::Path(this), Pat::Path(other)) => this == other,
            (Pat::Range(this), Pat::Range(other)) => this == other,
            (Pat::Reference(this), Pat::Reference(other)) => this == other,
            (Pat::Rest(this), Pat::Rest(other)) => this == other,
            (Pat::Slice(this), Pat::Slice(other)) => this == other,
            (Pat::Struct(this), Pat::Struct(other)) => this == other,
            (Pat::Tuple(this), Pat::Tuple(other)) => this == other,
            (Pat::TupleStruct(this), Pat::TupleStruct(other)) => this == other,
            (Pat::Type(this), Pat::Type(other)) => this == other,
            (Pat::Verbatim(this), Pat::Verbatim(other)) => {
                TokenStreamHelper(this) == TokenStreamHelper(other)
            }
            (Pat::Wild(this), Pat::Wild(other)) => this == other,
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for Pat {
    fn hash<H>(&self, hash: &mut H)
    where
        H: Hasher,
    {
        match self {
            Pat::Box(pat) => {
                hash.write_u8(0);
                pat.hash(hash);
            }
            Pat::Ident(pat) => {
                hash.write_u8(1);
                pat.hash(hash);
            }
            Pat::Lit(pat) => {
                hash.write_u8(2);
                pat.hash(hash);
            }
            Pat::Macro(pat) => {
                hash.write_u8(3);
                pat.hash(hash);
            }
            Pat::Or(pat) => {
                hash.write_u8(4);
                pat.hash(hash);
            }
            Pat::Path(pat) => {
                hash.write_u8(5);
                pat.hash(hash);
            }
            Pat::Range(pat) => {
                hash.write_u8(6);
                pat.hash(hash);
            }
            Pat::Reference(pat) => {
                hash.write_u8(7);
                pat.hash(hash);
            }
            Pat::Rest(pat) => {
                hash.write_u8(8);
                pat.hash(hash);
            }
            Pat::Slice(pat) => {
                hash.write_u8(9);
                pat.hash(hash);
            }
            Pat::Struct(pat) => {
                hash.write_u8(10);
                pat.hash(hash);
            }
            Pat::Tuple(pat) => {
                hash.write_u8(11);
                pat.hash(hash);
            }
            Pat::TupleStruct(pat) => {
                hash.write_u8(12);
                pat.hash(hash);
            }
            Pat::Type(pat) => {
                hash.write_u8(13);
                pat.hash(hash);
            }
            Pat::Verbatim(pat) => {
                hash.write_u8(14);
                TokenStreamHelper(pat).hash(hash);
            }
            Pat::Wild(pat) => {
                hash.write_u8(15);
                pat.hash(hash);
            }
            Pat::__Nonexhaustive => unreachable!(),
        }
    }
}

#[cfg(feature = "parsing")]
mod parsing {
    use super::*;

    use crate::ext::IdentExt;
    use crate::parse::{Parse, ParseStream, Result};
    use crate::path;

    impl Member {
        fn is_unnamed(&self) -> bool {
            match *self {
                Member::Named(_) => false,
                Member::Unnamed(_) => true,
            }
        }
    }

    // We need some way to persist whether or not we are in a closure between functions without having
    // to pass an fn pointer to each function telling it which to use.
    struct PatParseContext {
        parse_fn: fn(ParseStream, &Self) -> Result<Pat>,
    }

    impl PatParseContext {
        fn parse_inner(input: ParseStream, cx: &Self) -> Result<Pat> {
            let lookahead = input.lookahead1();
            if lookahead.peek(Ident)
                && ({
                    input.peek2(Token![::])
                        || input.peek2(Token![!])
                        || input.peek2(token::Brace)
                        || input.peek2(token::Paren)
                        || input.peek2(Token![..])
                            && !{
                                let ahead = input.fork();
                                ahead.parse::<Ident>()?;
                                ahead.parse::<RangeLimits>()?;
                                ahead.is_empty() || ahead.peek(Token![,])
                            }
                })
                || input.peek(Token![self]) && input.peek2(Token![::])
                || lookahead.peek(Token![::])
                || lookahead.peek(Token![<])
                || input.peek(Token![Self])
                || input.peek(Token![super])
                || input.peek(Token![extern])
                || input.peek(Token![crate])
            {
                cx.pat_path_or_macro_or_struct_or_range(input)
            } else if lookahead.peek(Token![_]) {
                cx.pat_wild(input).map(Pat::Wild)
            } else if input.peek(Token![box]) {
                cx.pat_box(input).map(Pat::Box)
            } else if input.peek(Token![-]) || lookahead.peek(Lit) {
                cx.pat_lit_or_range(input)
            } else if lookahead.peek(Token![ref])
                || lookahead.peek(Token![mut])
                || input.peek(Token![self])
                || input.peek(Ident)
            {
                cx.pat_ident(input).map(Pat::Ident)
            } else if lookahead.peek(Token![&]) {
                cx.pat_reference(input).map(Pat::Reference)
            } else if lookahead.peek(token::Paren) {
                cx.pat_tuple(input).map(Pat::Tuple)
            } else if lookahead.peek(token::Bracket) {
                cx.pat_slice(input).map(Pat::Slice)
            } else if lookahead.peek(Token![..]) && !input.peek(Token![...]) {
                cx.pat_rest(input).map(Pat::Rest)
            } else {
                Err(lookahead.error())
            }
        }

        /// This function adds functionality to parse a PatOr. It is important that this is not called
        /// when parsing a closure argument because this function works by parsing a Token![|] if the
        /// following ParseStream can be parsed into a Pat.
        ///
        /// In a closure declaration most of the time the closing Token![|] can be followed directly
        /// by an Ident, Lit, Path, etc. and in that case we do not want to consume the closing Token![|].
        fn parse_with_or(input: ParseStream, cx: &Self) -> Result<Pat> {
            let mut cases = Punctuated::new();

            let leading_vert = if input.peek(Token![|]) {
                Some(input.parse::<Token![|]>()?)
            } else {
                None
            };

            loop {
                cases.push_value(Self::parse_inner(input, cx)?);
                // If the next token is not a pipe, then break. We've reached the end of the pattern.
                if !input.peek(Token![|]) {
                    break;
                }
                // Even if the second next is a pipe we don't want to consume it until we know it is
                // followed by another pattern. Otherwise we would consume the closing pipe in a closure
                // and parse further than we're supposed to.
                let forked = &input.fork();
                forked.parse::<Token![|]>()?;
                match Self::parse_inner(forked, cx) {
                    Ok(_) => cases.push_punct(input.parse()?),
                    Err(_) => break,
                }
            }

            Ok(if cases.len() == 1 {
                cases.pop().unwrap().into_value()
            } else {
                Pat::Or(PatOr {
                    attrs: Vec::new(),
                    leading_vert,
                    cases,
                })
            })
        }

        // pub fn parse_closure_arg(input: ParseStream) -> Result<Pat> {
        //     // Self { parse_fn: self.parse_inner }::parse(input)
        //     Ok(())
        // }

        fn pat_path_or_macro_or_struct_or_range(&self, input: ParseStream) -> Result<Pat> {
            let (qself, path) = path::parsing::qpath(input, true)?;

            if input.peek(Token![..]) {
                return self.pat_range(input, qself, path).map(Pat::Range);
            }

            if qself.is_some() {
                return Ok(Pat::Path(PatPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                }));
            }

            if input.peek(Token![!]) && !input.peek(Token![!=]) {
                let mut contains_arguments = false;
                for segment in &path.segments {
                    match segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(_) | PathArguments::Parenthesized(_) => {
                            contains_arguments = true;
                        }
                    }
                }

                if !contains_arguments {
                    let bang_token: Token![!] = input.parse()?;
                    let (delimiter, tokens) = mac::parse_delimiter(input)?;
                    return Ok(Pat::Macro(PatMacro {
                        attrs: Vec::new(),
                        mac: Macro {
                            path,
                            bang_token,
                            delimiter,
                            tokens,
                        },
                    }));
                }
            }

            if input.peek(token::Brace) {
                self.pat_struct(input, path).map(Pat::Struct)
            } else if input.peek(token::Paren) {
                self.pat_tuple_struct(input, path).map(Pat::TupleStruct)
            } else if input.peek(Token![..]) {
                self.pat_range(input, qself, path).map(Pat::Range)
            } else {
                Ok(Pat::Path(PatPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                }))
            }
        }

        fn pat_wild(&self, input: ParseStream) -> Result<PatWild> {
            Ok(PatWild {
                attrs: Vec::new(),
                underscore_token: input.parse()?,
            })
        }

        fn pat_box(&self, input: ParseStream) -> Result<PatBox> {
            Ok(PatBox {
                attrs: Vec::new(),
                box_token: input.parse()?,
                pat: Box::new((self.parse_fn)(input, self)?),
            })
        }

        fn pat_ident(&self, input: ParseStream) -> Result<PatIdent> {
            Ok(PatIdent {
                attrs: Vec::new(),
                by_ref: input.parse()?,
                mutability: input.parse()?,
                ident: input.call(Ident::parse_any)?,
                subpat: {
                    if input.peek(Token![@]) {
                        let at_token: Token![@] = input.parse()?;
                        let subpat: Pat = (self.parse_fn)(input, self)?;
                        Some((at_token, Box::new(subpat)))
                    } else {
                        None
                    }
                },
            })
        }

        fn pat_tuple_struct(&self, input: ParseStream, path: Path) -> Result<PatTupleStruct> {
            Ok(PatTupleStruct {
                attrs: Vec::new(),
                path,
                pat: self.pat_tuple(input)?,
            })
        }

        fn pat_struct(&self, input: ParseStream, path: Path) -> Result<PatStruct> {
            let content;
            let brace_token = braced!(content in input);

            let mut fields = Punctuated::new();
            while !content.is_empty() && !content.peek(Token![..]) {
                let value = self.field_pat(&content)?;
                fields.push_value(value);
                if !content.peek(Token![,]) {
                    break;
                }
                let punct: Token![,] = content.parse()?;
                fields.push_punct(punct);
            }

            let dot2_token = if fields.empty_or_trailing() && content.peek(Token![..]) {
                Some(content.parse()?)
            } else {
                None
            };

            Ok(PatStruct {
                attrs: Vec::new(),
                path,
                brace_token,
                fields,
                dot2_token,
            })
        }

        fn field_pat(&self, input: ParseStream) -> Result<FieldPat> {
            let attrs = input.call(Attribute::parse_outer)?;
            let boxed: Option<Token![box]> = input.parse()?;
            let by_ref: Option<Token![ref]> = input.parse()?;
            let mutability: Option<Token![mut]> = input.parse()?;
            let member: Member = input.parse()?;

            if boxed.is_none() && by_ref.is_none() && mutability.is_none() && input.peek(Token![:])
                || member.is_unnamed()
            {
                return Ok(FieldPat {
                    attrs,
                    member,
                    colon_token: input.parse()?,
                    pat: Box::new((self.parse_fn)(input, self)?),
                });
            }

            let ident = match member {
                Member::Named(ident) => ident,
                Member::Unnamed(_) => unreachable!(),
            };

            let mut pat = Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref,
                mutability,
                ident: ident.clone(),
                subpat: None,
            });

            if let Some(boxed) = boxed {
                pat = Pat::Box(PatBox {
                    attrs: Vec::new(),
                    box_token: boxed,
                    pat: Box::new(pat),
                });
            }

            Ok(FieldPat {
                attrs,
                member: Member::Named(ident),
                colon_token: None,
                pat: Box::new(pat),
            })
        }

        fn pat_range(&self, input: ParseStream, qself: Option<QSelf>, path: Path) -> Result<PatRange> {
            Ok(PatRange {
                attrs: Vec::new(),
                lo: Box::new(Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself,
                    path,
                })),
                limits: input.parse()?,
                hi: self.pat_lit_expr(input)?,
            })
        }

        fn pat_tuple(&self, input: ParseStream) -> Result<PatTuple> {
            let content;
            let paren_token = parenthesized!(content in input);

            let mut elems = Punctuated::new();
            while !content.is_empty() {
                let value: Pat = (self.parse_fn)(&content, self)?;
                elems.push_value(value);
                if content.is_empty() {
                    break;
                }
                let punct = content.parse()?;
                elems.push_punct(punct);
            }

            Ok(PatTuple {
                attrs: Vec::new(),
                paren_token,
                elems,
            })
        }

        fn pat_reference(&self, input: ParseStream) -> Result<PatReference> {
            Ok(PatReference {
                attrs: Vec::new(),
                and_token: input.parse()?,
                mutability: input.parse()?,
                pat: Box::new((self.parse_fn)(input, self)?),
            })
        }

        fn pat_lit_or_range(&self, input: ParseStream) -> Result<Pat> {
            let lo = self.pat_lit_expr(input)?;
            if input.peek(Token![..]) {
                Ok(Pat::Range(PatRange {
                    attrs: Vec::new(),
                    lo,
                    limits: input.parse()?,
                    hi: self.pat_lit_expr(input)?,
                }))
            } else {
                Ok(Pat::Lit(PatLit {
                    attrs: Vec::new(),
                    expr: lo,
                }))
            }
        }

        fn pat_lit_expr(&self, input: ParseStream) -> Result<Box<Expr>> {
            let neg: Option<Token![-]> = input.parse()?;

            let lookahead = input.lookahead1();
            let expr = if lookahead.peek(Lit) {
                Expr::Lit(input.parse()?)
            } else if lookahead.peek(Ident)
                || lookahead.peek(Token![::])
                || lookahead.peek(Token![<])
                || lookahead.peek(Token![self])
                || lookahead.peek(Token![Self])
                || lookahead.peek(Token![super])
                || lookahead.peek(Token![extern])
                || lookahead.peek(Token![crate])
            {
                Expr::Path(input.parse()?)
            } else {
                return Err(lookahead.error());
            };

            Ok(Box::new(if let Some(neg) = neg {
                Expr::Unary(ExprUnary {
                    attrs: Vec::new(),
                    op: UnOp::Neg(neg),
                    expr: Box::new(expr),
                })
            } else {
                expr
            }))
        }

        fn pat_slice(&self, input: ParseStream) -> Result<PatSlice> {
            let content;
            let bracket_token = bracketed!(content in input);

            let mut elems = Punctuated::new();
            while !content.is_empty() {
                let value: Pat = (self.parse_fn)(&content, self)?;
                elems.push_value(value);
                if content.is_empty() {
                    break;
                }
                let punct = content.parse()?;
                elems.push_punct(punct);
            }

            Ok(PatSlice {
                attrs: Vec::new(),
                bracket_token,
                elems,
            })
        }

        fn pat_rest(&self, input: ParseStream) -> Result<PatRest> {
            Ok(PatRest {
                attrs: Vec::new(),
                dot2_token: input.parse()?,
            })
        }
    }

    impl Pat {
        fn parse(input: ParseStream) -> Result<Pat> {
            let cx = PatParseContext {
                parse_fn: PatParseContext::parse_with_or,
            };
            (cx.parse_fn)(input, &cx)
        }

        pub fn parse_closure_arg(input: ParseStream) -> Result<Pat> {
            let cx = PatParseContext {
                parse_fn: PatParseContext::parse_inner,
            };
            (cx.parse_fn)(input, &cx)
        }
    }

    impl Parse for Pat {
        fn parse(input: ParseStream) -> Result<Self> {
            Self::parse(input)
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;

    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    use crate::attr::FilterAttrs;

    impl ToTokens for PatWild {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.underscore_token.to_tokens(tokens);
        }
    }

    impl ToTokens for PatIdent {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.by_ref.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if let Some((at_token, subpat)) = &self.subpat {
                at_token.to_tokens(tokens);
                subpat.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for PatStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.path.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.fields.to_tokens(tokens);
                // NOTE: We need a comma before the dot2 token if it is present.
                if !self.fields.empty_or_trailing() && self.dot2_token.is_some() {
                    <Token![,]>::default().to_tokens(tokens);
                }
                self.dot2_token.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for PatTupleStruct {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.path.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatType {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.pat.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for PatPath {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            private::print_path(tokens, &self.qself, &self.path);
        }
    }

    impl ToTokens for PatTuple {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.paren_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for PatBox {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.box_token.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatReference {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.and_token.to_tokens(tokens);
            self.mutability.to_tokens(tokens);
            self.pat.to_tokens(tokens);
        }
    }

    impl ToTokens for PatRest {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.dot2_token.to_tokens(tokens);
        }
    }

    impl ToTokens for PatLit {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.expr.to_tokens(tokens);
        }
    }

    impl ToTokens for PatRange {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.lo.to_tokens(tokens);
            match &self.limits {
                RangeLimits::HalfOpen(t) => t.to_tokens(tokens),
                RangeLimits::Closed(t) => t.to_tokens(tokens),
            }
            self.hi.to_tokens(tokens);
        }
    }

    impl ToTokens for PatSlice {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.bracket_token.surround(tokens, |tokens| {
                self.elems.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for PatMacro {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.mac.to_tokens(tokens);
        }
    }

    impl ToTokens for PatOr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            self.leading_vert.to_tokens(tokens);
            self.cases.to_tokens(tokens);
        }
    }

    impl ToTokens for FieldPat {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.outer());
            if let Some(colon_token) = &self.colon_token {
                self.member.to_tokens(tokens);
                colon_token.to_tokens(tokens);
            }
            self.pat.to_tokens(tokens);
        }
    }
}
