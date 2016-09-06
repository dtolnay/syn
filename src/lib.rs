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

mod common;
pub use common::{
    Ident,
    Visibility,
};

mod generics;
pub use generics::{
    Generics,
    Lifetime,
    LifetimeDef,
    TyParam,
    TyParamBound,
    WhereBoundPredicate,
    WherePredicate,
    WhereRegionPredicate,
};

mod item;
pub use item::{
    Body,
    Field,
    Item,
    Style,
    Variant,
};

mod ty;
pub use ty::{
    AngleBracketedParameterData,
    Arg,
    BareFnTy,
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

#[cfg(feature = "parsing")]
pub fn parse(input: &str) -> Item {
    return match item::parsing::item(input) {
        nom::IResult::Done(rest, ast) => {
            if rest.is_empty() {
                ast
            } else {
                panic!("more than a single input item: {:?}", rest)
            }
        }
        nom::IResult::Error(err) => raise(err),
        nom::IResult::Incomplete(_) => panic!("incomplete input item"),
    };

    fn raise(mut err: nom::Err<&str>) -> ! {
        loop {
            match err {
                nom::Err::Code(kind) => {
                    panic!("failed to parse {:?}", kind)
                }
                nom::Err::Position(kind, pos) => {
                    panic!("failed to parse {:?}: {:?}", kind, pos)
                }
                nom::Err::Node(_, next) |
                nom::Err::NodePosition(_, _, next) => {
                    err = *next;
                }
            }
        }
    }
}
