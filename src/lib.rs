#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

#[cfg(feature = "printing")]
extern crate quote;

#[cfg(feature = "parsing")]
extern crate unicode_xid;

#[cfg(feature = "parsing")]
#[macro_use]
mod nom;

#[macro_use]
mod helper;

mod escape;

mod attr;
pub use attr::{
    Attribute,
    MetaItem,
};

mod data;
pub use data::{
    Discriminant,
    Field,
    Variant,
    VariantData,
    Visibility,
};

#[cfg(feature = "full")]
mod expr;
#[cfg(feature = "full")]
pub use expr::{
    Arm,
    BinOp,
    BindingMode,
    Block,
    BlockCheckMode,
    CaptureBy,
    Expr,
    FieldPat,
    Local,
    MacStmtStyle,
    Pat,
    RangeLimits,
    Stmt,
    UnOp,
};

mod generics;
pub use generics::{
    Generics,
    Lifetime,
    LifetimeDef,
    TraitBoundModifier,
    TyParam,
    TyParamBound,
    WhereBoundPredicate,
    WhereClause,
    WherePredicate,
    WhereRegionPredicate,
};

mod ident;
pub use ident::{
    Ident,
};

#[cfg(feature = "full")]
mod item;
#[cfg(feature = "full")]
pub use item::{
    Abi,
    Constness,
    Defaultness,
    ForeignItemKind,
    ForeignItem,
    ForeignMod,
    ImplItem,
    ImplItemKind,
    ImplPolarity,
    Item,
    ItemKind,
    MethodSig,
    PathListItem,
    TraitItem,
    TraitItemKind,
    Unsafety,
    ViewPath,
};

mod lit;
pub use lit::{
    FloatTy,
    IntTy,
    Lit,
    StrStyle,
};

#[cfg(feature = "full")]
mod mac;
#[cfg(feature = "full")]
pub use mac::{
    BinOpToken,
    DelimToken,
    Delimited,
    Mac,
    SequenceRepetition,
    Token,
    TokenTree,
};

mod macro_input;
pub use macro_input::{
    Body,
    MacroInput,
};

mod ty;
pub use ty::{
    AngleBracketedParameterData,
    BareFnTy,
    FnArg,
    FnDecl,
    FunctionRetTy,
    MutTy,
    Mutability,
    ParenthesizedParameterData,
    Path,
    PathParameters,
    PathSegment,
    PolyTraitRef,
    QSelf,
    Ty,
    TypeBinding,
};

#[cfg(feature = "aster")]
pub mod aster;

#[cfg(feature = "visit")]
pub mod visit;

#[cfg(feature = "parsing")]
pub use parsing::*;

#[cfg(feature = "parsing")]
mod parsing {
    use super::*;
    use {generics, macro_input, ty};
    use nom;

    #[cfg(feature = "full")]
    use {expr, item};

    pub fn parse_macro_input(input: &str) -> Result<MacroInput, String> {
        unwrap("macro input", macro_input::parsing::macro_input, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_item(input: &str) -> Result<Item, String> {
        unwrap("item", item::parsing::item, input)
    }

    #[cfg(feature = "full")]
    pub fn parse_expr(input: &str) -> Result<Expr, String> {
        unwrap("expression", expr::parsing::expr, input)
    }

    pub fn parse_type(input: &str) -> Result<Ty, String> {
        unwrap("type", ty::parsing::ty, input)
    }

    pub fn parse_path(input: &str) -> Result<Path, String> {
        unwrap("path", ty::parsing::path, input)
    }

    pub fn parse_where_clause(input: &str) -> Result<WhereClause, String> {
        unwrap("where clause", generics::parsing::where_clause, input)
    }

    fn unwrap<T>(name: &'static str, f: fn(&str) -> nom::IResult<&str, T>, input: &str) -> Result<T, String> {
        match f(input) {
            nom::IResult::Done(rest, t) => {
                if rest.is_empty() {
                    Ok(t)
                } else {
                    Err(format!("remaining tokens after {}: {:?}", name, rest))
                }
            }
            nom::IResult::Error => Err(format!("failed to parse {}: {:?}", name, input)),
        }
    }
}
