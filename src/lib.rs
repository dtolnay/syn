#[cfg(feature = "printing")]
extern crate quote;

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

mod item;
pub use item::{
    Body,
    Field,
    Item,
    Variant,
    VariantData,
    Visibility,
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
    use {generics, item, ty};
    use nom;

    pub fn parse_item(input: &str) -> Result<Item, String> {
        unwrap("item", item::parsing::item, input)
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
