#![doc(html_root_url = "https://dtolnay.github.io/syn")]

#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

extern crate proc_macro2;
extern crate unicode_xid;

#[cfg(feature = "printing")]
extern crate quote;

#[cfg_attr(feature = "parsing", macro_use)]
extern crate synom;

#[macro_use]
mod macros;

mod attr;
pub use attr::{Attribute, AttrStyle, MetaItem, NestedMetaItem, MetaItemList,
               MetaNameValue};

mod constant;
pub use constant::{ConstExpr, ConstCall, ConstBinary, ConstUnary, ConstCast,
                   ConstIndex, ConstParen};

mod data;
pub use data::{Field, Variant, VariantData, Visibility, VisRestricted, VisCrate,
               VisPublic, VisInherited};

#[cfg(feature = "full")]
mod expr;
#[cfg(feature = "full")]
pub use expr::{Arm, BindingMode, Block, CaptureBy, Expr, ExprKind, FieldPat, FieldValue, Local,
               MacStmtStyle, Pat, RangeLimits, Stmt, ExprBox, ExprInPlace,
               ExprArray, ExprCall, ExprMethodCall, ExprTup, ExprBinary, ExprUnary,
               ExprCast, ExprType, ExprIf, ExprIfLet, ExprWhile, ExprWhileLet,
               ExprForLoop, ExprLoop, ExprMatch, ExprClosure, ExprBlock,
               ExprAssign, ExprAssignOp, ExprField, ExprTupField, ExprIndex,
               ExprRange, ExprPath, ExprAddrOf, ExprBreak, ExprContinue,
               ExprRet, ExprStruct, ExprRepeat, ExprParen, ExprTry, ExprCatch,
               PatIdent, PatWild, PatStruct, PatTuple, PatTupleStruct, PatPath,
               PatBox, PatRef, PatLit, PatRange, PatSlice};

mod generics;
pub use generics::{Generics, Lifetime, LifetimeDef, TraitBoundModifier, TyParam, TyParamBound,
                   WhereBoundPredicate, WhereClause, WhereEqPredicate, WherePredicate,
                   WhereRegionPredicate, BoundLifetimes};
#[cfg(feature = "printing")]
pub use generics::{ImplGenerics, Turbofish, TyGenerics};

mod ident;
pub use ident::Ident;

#[cfg(feature = "full")]
mod item;
#[cfg(feature = "full")]
pub use item::{Constness, Defaultness, FnArg, FnDecl, ForeignItemKind, ForeignItem, ItemForeignMod,
               ImplItem, ImplItemKind, ImplPolarity, Item, ItemKind, MethodSig, PathListItem,
               TraitItem, TraitItemKind, ViewPath, ItemExternCrate, ItemUse,
               ItemStatic, ItemConst, ItemFn, ItemMod, ItemTy, ItemEnum,
               ItemStruct, ItemUnion, ItemTrait, ItemDefaultImpl, ItemImpl,
               PathSimple, PathGlob, PathList, ForeignItemFn, ForeignItemStatic,
               TraitItemConst, TraitItemMethod, TraitItemType,
               ImplItemConst, ImplItemMethod, ImplItemType, ArgSelfRef,
               ArgSelf, ArgCaptured};

#[cfg(feature = "full")]
mod krate;
#[cfg(feature = "full")]
pub use krate::Crate;

mod lit;
pub use lit::{Lit, LitKind};

mod mac;
pub use mac::{Mac, TokenTree};

mod derive;
pub use derive::{Body, DeriveInput, BodyEnum, BodyStruct};
// Deprecated. Use `DeriveInput` instead.
#[doc(hidden)]
pub type MacroInput = DeriveInput;

mod op;
pub use op::{BinOp, UnOp};

mod ty;
pub use ty::{Abi, AngleBracketedParameterData, BareFnArg, BareFnTy, FunctionRetTy, MutTy,
             Mutability, ParenthesizedParameterData, Path, PathParameters, PathSegment,
             PolyTraitRef, QSelf, Ty, TypeBinding, Unsafety, TySlice, TyArray,
             TyPtr, TyRptr, TyBareFn, TyNever, TyTup, TyPath, TyTraitObject,
             TyImplTrait, TyParen, TyInfer};
#[cfg(feature = "printing")]
pub use ty::PathTokens;

pub use synom::span::Span;
pub use synom::tokens;
pub use synom::delimited;

#[cfg(feature = "visit")]
pub mod visit;

#[cfg(feature = "fold")]
pub mod fold;

#[cfg(feature = "parsing")]
mod parsing {
    use std::str::FromStr;

    use super::*;
    use synom::{Synom, ParseError};
    use proc_macro2::TokenStream;

    macro_rules! traits {
        ($($ty:ident,)*) => ($(
            impl From<TokenStream> for $ty {
                fn from(stream: TokenStream) -> $ty {
                    $ty::parse_all_unwrap(stream)
                }
            }

            impl FromStr for $ty {
                type Err = ParseError;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    $ty::parse_str_all(s)
                }
            }
        )*)
    }

    traits! {
        DeriveInput,
        TyParamBound,
        Ident,
        WhereClause,
        Ty,
        Lit,
    }

    #[cfg(feature = "full")]
    traits! {
        Expr,
        Item,
        Crate,
    }
}
