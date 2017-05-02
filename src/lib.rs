#![doc(html_root_url = "https://dtolnay.github.io/syn")]

#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

#[cfg(feature = "printing")]
extern crate quote;

#[cfg(feature = "parsing")]
extern crate unicode_xid;

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

#[cfg(feature = "parsing")]
mod escape;

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
    use synom::{space, IResult};

    #[cfg(feature = "full")]
    use {expr, item, krate};

    /// Parse the stringified representation of a struct or enum passed
    /// to a `proc_macro_derive` function.
    pub fn parse_derive_input(input: &str) -> Result<DeriveInput, String> {
        unwrap("derive input", derive::parsing::derive_input, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_crate(input: &str) -> Result<Crate, String> {
        unwrap("crate", krate::parsing::krate, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_item(input: &str) -> Result<Item, String> {
        unwrap("item", item::parsing::item, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_items(input: &str) -> Result<Vec<Item>, String> {
        unwrap("items", item::parsing::items, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_expr(input: &str) -> Result<Expr, String> {
        unwrap("expression", expr::parsing::expr, input)
    }

    pub fn parse_type(input: &str) -> Result<Ty, String> {
        unwrap("type", ty::parsing::ty, input)
    }

    /// Parse a path, such as `std::str::FromStr` or `::syn::parse_path`.
    pub fn parse_path(input: &str) -> Result<Path, String> {
        unwrap("path", ty::parsing::path, input)
    }

    pub fn parse_where_clause(input: &str) -> Result<WhereClause, String> {
        unwrap("where clause", generics::parsing::where_clause, input)
    }

    pub fn parse_token_trees(input: &str) -> Result<Vec<TokenTree>, String> {
        unwrap("token trees", mac::parsing::token_trees, input)
    }

    pub fn parse_ident(input: &str) -> Result<Ident, String> {
        unwrap("identifier", ident::parsing::ident, input)
    }

    pub fn parse_ty_param_bound(input: &str) -> Result<TyParamBound, String> {
        unwrap("type parameter bound",
               generics::parsing::ty_param_bound,
               input)
    }

    /// Parse an attribute declared outside the item it annotates, such as
    /// a struct annotation. They are written as `#[...]`.
    pub fn parse_outer_attr(input: &str) -> Result<Attribute, String> {
        unwrap("outer attribute", attr::parsing::outer_attr, input)
    }

    /// Parse an attribute declared inside the item it annotates. These are used
    /// for crate annotations or for mod-level declarations when modules are in
    /// their own files. They are written as `#![...]`.
    #[cfg(feature = "full")]
    pub fn parse_inner_attr(input: &str) -> Result<Attribute, String> {
        unwrap("inner attribute", attr::parsing::inner_attr, input)
    }

    /// Deprecated: Use `parse_derive_input` instead.
    #[doc(hidden)]
    #[deprecated(since="0.11.0", note = "Use `parse_derive_input` instead")]
    pub fn parse_macro_input(input: &str) -> Result<MacroInput, String> {
        parse_derive_input(input)
    }

    /// Alias for `syn::parse_derive_input`.
    impl FromStr for DeriveInput {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_derive_input(s)
        }
    }

    /// Alias for `syn::parse_crate`.
    #[cfg(feature = "full")]
    impl FromStr for Crate {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_crate(s)
        }
    }

    /// Alias for `syn::parse_item`.
    #[cfg(feature = "full")]
    impl FromStr for Item {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_item(s)
        }
    }

    /// Alias for `syn::parse_expr`.
    #[cfg(feature = "full")]
    impl FromStr for Expr {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_expr(s)
        }
    }

    /// Alias for `syn::parse_type`.
    impl FromStr for Ty {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_type(s)
        }
    }

    /// Alias for `syn::parse_path`.
    impl FromStr for Path {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_path(s)
        }
    }

    /// Alias for `syn::parse_where_clause`.
    impl FromStr for WhereClause {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_where_clause(s)
        }
    }

    /// Alias for `syn::parse_ident`.
    impl FromStr for Ident {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_ident(s)
        }
    }

    /// Alias for `syn::parse_ty_param_bound`.
    impl FromStr for TyParamBound {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            parse_ty_param_bound(s)
        }
    }

    fn unwrap<T>(name: &'static str,
                 f: fn(&str) -> IResult<&str, T>,
                 input: &str)
                 -> Result<T, String> {
        match f(input) {
            IResult::Done(mut rest, t) => {
                rest = space::skip_whitespace(rest);
                if rest.is_empty() {
                    Ok(t)
                } else if rest.len() == input.len() {
                    // parsed nothing
                    Err(format!("failed to parse {}: {:?}", name, rest))
                } else {
                    Err(format!("unparsed tokens after {}: {:?}", name, rest))
                }
            }
            IResult::Error => Err(format!("failed to parse {}: {:?}", name, input)),
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
