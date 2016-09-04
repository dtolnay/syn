use super::*;

use common::word;
use ty::{ty, poly_trait_ref};
use nom::multispace;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Generics {
    pub lifetimes: Vec<LifetimeDef>,
    pub ty_params: Vec<TyParam>,
    pub where_clause: Vec<WherePredicate>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lifetime {
    pub ident: Ident,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LifetimeDef {
    pub lifetime: Lifetime,
    pub bounds: Vec<Lifetime>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TyParam {
    pub ident: Ident,
    pub bounds: Vec<TyParamBound>,
    pub default: Option<Ty>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TyParamBound {
    MaybeSized,
    Region(Lifetime),
    Trait(PolyTraitRef),
}

/// A single predicate in a `where` clause
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum WherePredicate {
    /// A type binding, e.g. `for<'c> Foo: Send+Clone+'c`
    BoundPredicate(WhereBoundPredicate),
    /// A lifetime predicate, e.g. `'a: 'b+'c`
    RegionPredicate(WhereRegionPredicate),
}

/// A type bound.
///
/// E.g. `for<'c> Foo: Send+Clone+'c`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhereBoundPredicate {
    /// Any lifetimes from a `for` binding
    pub bound_lifetimes: Vec<LifetimeDef>,
    /// The type being bounded
    pub bounded_ty: Ty,
    /// Trait and lifetime bounds (`Clone+Send+'static`)
    pub bounds: Vec<TyParamBound>,
}

/// A lifetime predicate.
///
/// E.g. `'a: 'b+'c`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhereRegionPredicate {
    pub lifetime: Lifetime,
    pub bounds: Vec<Lifetime>,
}

named!(pub generics<&str, Generics>, chain!(
    bracketed: alt!(
        chain!(
            punct!("<") ~
            lifetimes: separated_list!(punct!(","), lifetime_def) ~
            ty_params: opt_vec!(preceded!(
                cond!(!lifetimes.is_empty(), punct!(",")),
                separated_nonempty_list!(punct!(","), ty_param)
            )) ~
            punct!(">"),
            move || (lifetimes, ty_params)
        )
        |
        epsilon!() => { |_| (Vec::new(), Vec::new()) }
    ) ~
    where_clause: opt_vec!(chain!(
        punct!("where") ~
        multispace ~
        predicates: separated_nonempty_list!(punct!(","), where_predicate) ~
        punct!(",")? ~
        move || predicates
    )),
    move || Generics {
        lifetimes: bracketed.0,
        ty_params: bracketed.1,
        where_clause: where_clause,
    }
));

named!(pub lifetime<&str, Lifetime>, preceded!(
    punct!("'"),
    map!(word, |ident| Lifetime { ident: ident })
));

named!(pub lifetime_def<&str, LifetimeDef>, chain!(
    life: lifetime ~
    bounds: opt_vec!(preceded!(
        punct!(":"),
        separated_nonempty_list!(punct!(","), lifetime)
    )),
    move || LifetimeDef {
        lifetime: life,
        bounds: bounds,
    }
));

named!(pub bound_lifetimes<&str, Vec<LifetimeDef> >, opt_vec!(chain!(
    punct!("for") ~
    punct!("<") ~
    lifetimes: separated_list!(punct!(","), lifetime_def) ~
    punct!(">"),
    move || lifetimes
)));

named!(ty_param<&str, TyParam>, chain!(
    ident: word ~
    bounds: opt_vec!(preceded!(
        punct!(":"),
        separated_nonempty_list!(punct!("+"), ty_param_bound)
    )) ~
    default: opt!(preceded!(
        punct!("="),
        ty
    )) ~
    move || TyParam {
        ident: ident,
        bounds: bounds,
        default: default,
    }
));

named!(pub ty_param_bound<&str, TyParamBound>, alt!(
    tuple!(punct!("?"), punct!("Sized")) => { |_| TyParamBound::MaybeSized }
    |
    lifetime => { TyParamBound::Region }
    |
    poly_trait_ref => { TyParamBound::Trait }
));

named!(where_predicate<&str, WherePredicate>, alt!(
    chain!(
        ident: lifetime ~
        punct!(":") ~
        bounds: separated_nonempty_list!(punct!("+"), lifetime),
        move || WherePredicate::RegionPredicate(WhereRegionPredicate {
            lifetime: ident,
            bounds: bounds,
        })
    )
    |
    chain!(
        bound_lifetimes: bound_lifetimes ~
        bounded_ty: ty ~
        punct!(":") ~
        bounds: separated_nonempty_list!(punct!("+"), ty_param_bound),
        move || WherePredicate::BoundPredicate(WhereBoundPredicate {
            bound_lifetimes: bound_lifetimes,
            bounded_ty: bounded_ty,
            bounds: bounds,
        })
    )
));
