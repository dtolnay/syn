#[cfg(feature = "parsing")]
#[macro_use]
extern crate nom;

#[cfg(feature = "printing")]
extern crate quote;

#[macro_use]
mod do_parse;

#[macro_use]
mod helper;

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
        unwrap("item", item::parsing::item(input))
    }

    pub fn parse_type(input: &str) -> Result<Ty, String> {
        unwrap("type", ty::parsing::ty(input))
    }

    pub fn parse_path(input: &str) -> Result<Path, String> {
        unwrap("path", ty::parsing::path(input))
    }

    pub fn parse_where_clause(input: &str) -> Result<WhereClause, String> {
        unwrap("where clause", generics::parsing::where_clause(input))
    }

    fn unwrap<T>(name: &'static str, ires: nom::IResult<&str, T>) -> Result<T, String> {
        return match ires {
            nom::IResult::Done(rest, t) => {
                if rest.is_empty() {
                    Ok(t)
                } else {
                    Err(format!("remaining tokens after {}: {:?}", name, rest))
                }
            }
            nom::IResult::Error(err) => Err(root_cause(err)),
            nom::IResult::Incomplete(_) => Err(format!("incomplete {}", name)),
        };

        fn root_cause(mut err: nom::Err<&str>) -> String {
            loop {
                match err {
                    nom::Err::Code(kind) => {
                        return format!("failed to parse {:?}", kind);
                    }
                    nom::Err::Position(kind, pos) => {
                        return format!("failed to parse {:?}: {:?}", kind, pos);
                    }
                    nom::Err::Node(_, next) |
                    nom::Err::NodePosition(_, _, next) => {
                        err = *next;
                    }
                }
            }
        }
    }
}
