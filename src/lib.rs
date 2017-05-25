#![doc(html_root_url = "https://dtolnay.github.io/syn")]

#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

#[cfg(feature = "printing")]
extern crate quote;

#[cfg(feature = "parsing")]
extern crate relex;

#[cfg(feature = "parsing")]
#[macro_use]
extern crate synom;

#[cfg(feature = "aster")]
pub mod aster;

mod attr;
pub use attr::{Attribute, AttrStyle, MetaItem, NestedMetaItem};

mod constant;
pub use constant::ConstExpr;

mod data;
pub use data::{Field, Variant, VariantData, Visibility};

#[cfg(feature = "full")]
mod expr;
#[cfg(feature = "full")]
pub use expr::{Arm, BindingMode, Block, CaptureBy, Expr, ExprKind, FieldPat, FieldValue, Local,
               MacStmtStyle, Pat, RangeLimits, Stmt};

mod generics;
pub use generics::{Generics, Lifetime, LifetimeDef, TraitBoundModifier, TyParam, TyParamBound,
                   WhereBoundPredicate, WhereClause, WhereEqPredicate, WherePredicate,
                   WhereRegionPredicate};
#[cfg(feature = "printing")]
pub use generics::{ImplGenerics, Turbofish, TyGenerics};

mod ident;
pub use ident::Ident;

#[cfg(feature = "full")]
mod item;
#[cfg(feature = "full")]
pub use item::{Constness, Defaultness, FnArg, FnDecl, ForeignItemKind, ForeignItem, ForeignMod,
               ImplItem, ImplItemKind, ImplPolarity, Item, ItemKind, MethodSig, PathListItem,
               TraitItem, TraitItemKind, ViewPath};

#[cfg(feature = "full")]
mod krate;
#[cfg(feature = "full")]
pub use krate::Crate;

mod lit;
pub use lit::{FloatTy, IntTy, Lit, StrStyle};
#[cfg(feature = "parsing")]
pub use lit::{ByteStrLit, FloatLit, IntLit, StrLit};

mod mac;
pub use mac::{BinOpToken, DelimToken, Delimited, Mac, Token, TokenTree};

mod derive;
pub use derive::{Body, DeriveInput};
// Deprecated. Use `DeriveInput` instead.
#[doc(hidden)]
pub type MacroInput = DeriveInput;

mod op;
pub use op::{BinOp, UnOp};

mod ty;
pub use ty::{Abi, AngleBracketedParameterData, BareFnArg, BareFnTy, FunctionRetTy, MutTy,
             Mutability, ParenthesizedParameterData, Path, PathParameters, PathSegment,
             PolyTraitRef, QSelf, Ty, TypeBinding, Unsafety};

#[cfg(feature = "visit")]
pub mod visit;

#[cfg(feature = "fold")]
pub mod fold;

#[cfg(feature = "parsing")]
pub use parsing::*;

#[cfg(feature = "parsing")]
mod parsing {
    use std::str::FromStr;

    use super::*;
    use {derive, generics, ident, mac, ty, attr};
    use synom::{IResult, TokenStream};

    use std::convert::From;
    use std::error::Error;
    use std::fmt;

    #[cfg(feature = "full")]
    use {expr, item, krate};

    #[derive(Debug)]
    pub struct ParseError(String);

    impl Error for ParseError {
        fn description(&self) -> &str {
            &self.0
        }
    }

    impl fmt::Display for ParseError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            <String as fmt::Display>::fmt(&self.0, f)
        }
    }

    impl From<synom::LexError> for ParseError {
        fn from(_: synom::LexError) -> ParseError {
            ParseError("error while lexing input string".to_owned())
        }
    }

    /// Parse the stringified representation of a struct or enum passed
    /// to a `proc_macro_derive` function.
    pub fn parse_derive_input(input: TokenStream) -> Result<DeriveInput, ParseError> {
        unwrap("derive input", derive::parsing::derive_input, input)
    }

    /// Parse an entire crate into an AST. This function takes a string as input
    /// instead of a TokenStream, as we need to handle parsing the BOM and
    /// shebang from the string.
    #[cfg(feature = "full")]
    pub fn parse_crate(mut input: &str) -> Result<Crate, ParseError> {
        // Strip the BOM if it is present
        const BOM: &str = "\u{feff}";
        if input.starts_with(BOM) {
            input = &input[BOM.len()..];
        }

        let mut shebang = None;
        if input.starts_with("#!") && !input.starts_with("#![") {
            if let Some(idx) = input.find('\n') {
                shebang = Some(input[..idx].to_string());
                input = &input[idx..];
            } else {
                shebang = Some(input.to_string());
                input = "";
            }
        }

        let mut krate = unwrap("crate", krate::parsing::krate,
                           input.parse()?)?;
        krate.shebang = shebang;
        Ok(krate)

    }

    #[cfg(feature = "full")]
    pub fn parse_item(input: TokenStream) -> Result<Item, ParseError> {
        unwrap("item", item::parsing::item, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_items(input: TokenStream) -> Result<Vec<Item>, ParseError> {
        unwrap("items", item::parsing::items, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_expr(input: TokenStream) -> Result<Expr, ParseError> {
        unwrap("expression", expr::parsing::expr, input)
    }

    pub fn parse_type(input: TokenStream) -> Result<Ty, ParseError> {
        unwrap("type", ty::parsing::ty, input)
    }

    /// Parse a path, such as `std::str::FromStr` or `::syn::parse_path`.
    pub fn parse_path(input: TokenStream) -> Result<Path, ParseError> {
        unwrap("path", ty::parsing::path, input)
    }

    pub fn parse_where_clause(input: TokenStream) -> Result<WhereClause, ParseError> {
        unwrap("where clause", generics::parsing::where_clause, input)
    }

    pub fn parse_token_trees(input: TokenStream) -> Result<Vec<TokenTree>, ParseError> {
        unwrap("token trees", mac::parsing::token_trees, input)
    }

    pub fn parse_ident(input: TokenStream) -> Result<Ident, ParseError> {
        unwrap("identifier", ident::parsing::ident, input)
    }

    pub fn parse_ty_param_bound(input: TokenStream) -> Result<TyParamBound, ParseError> {
        unwrap("type parameter bound",
               generics::parsing::ty_param_bound,
               input)
    }

    /// Parse an attribute declared outside the item it annotates, such as
    /// a struct annotation. They are written as `#[...]`.
    pub fn parse_outer_attr(input: TokenStream) -> Result<Attribute, ParseError> {
        unwrap("outer attribute", attr::parsing::outer_attr, input)
    }

    /// Parse an attribute declared inside the item it annotates. These are used
    /// for crate annotations or for mod-level declarations when modules are in
    /// their own files. They are written as `#![...]`.
    #[cfg(feature = "full")]
    pub fn parse_inner_attr(input: TokenStream) -> Result<Attribute, ParseError> {
        unwrap("inner attribute", attr::parsing::inner_attr, input)
    }

    /// Deprecated: Use `parse_derive_input` instead.
    #[doc(hidden)]
    #[deprecated(since="0.11.0", note = "Use `parse_derive_input` instead")]
    pub fn parse_macro_input(input: TokenStream) -> Result<MacroInput, ParseError> {
        parse_derive_input(input)
    }

    /// Alias for `syn::parse_derive_input`.
    impl FromStr for DeriveInput {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_derive_input(s.parse()?)
        }
    }

    /// Alias for `syn::parse_crate`.
    #[cfg(feature = "full")]
    impl FromStr for Crate {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_crate(s)
        }
    }

    /// Alias for `syn::parse_item`.
    #[cfg(feature = "full")]
    impl FromStr for Item {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_item(s.parse()?)
        }
    }

    /// Alias for `syn::parse_expr`.
    #[cfg(feature = "full")]
    impl FromStr for Expr {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_expr(s.parse()?)
        }
    }

    /// Alias for `syn::parse_type`.
    impl FromStr for Ty {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_type(s.parse()?)
        }
    }

    /// Alias for `syn::parse_path`.
    impl FromStr for Path {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_path(s.parse()?)
        }
    }

    /// Alias for `syn::parse_where_clause`.
    impl FromStr for WhereClause {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_where_clause(s.parse()?)
        }
    }

    /// Alias for `syn::parse_ident`.
    impl FromStr for Ident {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_ident(s.parse()?)
        }
    }

    /// Alias for `syn::parse_ty_param_bound`.
    impl FromStr for TyParamBound {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_ty_param_bound(s.parse()?)
        }
    }

    fn unwrap<T>(name: &'static str,
                 f: fn(&[synom::TokenTree]) -> IResult<&[synom::TokenTree], T>,
                 input: TokenStream)
                 -> Result<T, ParseError> {
        let input = synom::InputBuf::new(input);
        match f(&input) {
            IResult::Done(rest, t) => {
                if rest.is_empty() {
                    Ok(t)
                } else if rest.len() == input.len() {
                    // parsed nothing
                    Err(ParseError(format!("failed to parse {}", name)))
                } else {
                    Err(ParseError(format!("unparsed tokens after {}", name)))
                }
            }
            IResult::Error => Err(ParseError(format!("failed to parse {}", name))),
        }
    }
}

#[cfg(feature = "parsing")]
pub mod parse {
    //! This module contains a set of exported nom parsers which can be used to
    //! parse custom grammars when used alongside the `synom` crate.
    //!
    //! Internally, `syn` uses a fork of `nom` called `synom` which resolves a
    //! persistent pitfall of using `nom` to parse Rust by eliminating the
    //! `IResult::Incomplete` variant. The `synom` crate should be used instead
    //! of `nom` when working with the parsers in this module.

    pub use synom::IResult;

    #[cfg(feature = "full")]
    pub use item::parsing::item;

    #[cfg(feature = "full")]
    pub use expr::parsing::{expr, pat, block, stmt};

    pub use lit::parsing::{lit, string, byte_string, byte, character, float, int, boolean};

    pub use ty::parsing::{ty, path};

    pub use mac::parsing::token_tree as tt;

    pub use ident::parsing::ident;

    pub use generics::parsing::lifetime;
}
