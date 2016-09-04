use super::*;

#[cfg(feature = "parsing")]
use common::word;
#[cfg(feature = "parsing")]
use ty::{ty, poly_trait_ref};
#[cfg(feature = "parsing")]
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

#[cfg(feature = "parsing")]
named!(pub generics<&str, Generics>, do_parse!(
    bracketed: alt!(
        do_parse!(
            punct!("<") >>
            lifetimes: separated_list!(punct!(","), lifetime_def) >>
            ty_params: opt_vec!(preceded!(
                cond!(!lifetimes.is_empty(), punct!(",")),
                separated_nonempty_list!(punct!(","), ty_param)
            )) >>
            punct!(">") >>
            (lifetimes, ty_params)
        )
        |
        epsilon!() => { |_| (Vec::new(), Vec::new()) }
    ) >>
    where_clause: opt_vec!(do_parse!(
        punct!("where") >>
        multispace >>
        predicates: separated_nonempty_list!(punct!(","), where_predicate) >>
        opt!(punct!(",")) >>
        (predicates)
    )) >>
    (Generics {
        lifetimes: bracketed.0,
        ty_params: bracketed.1,
        where_clause: where_clause,
    })
));

#[cfg(feature = "parsing")]
named!(pub lifetime<&str, Lifetime>, preceded!(
    punct!("'"),
    map!(word, |ident| Lifetime { ident: ident })
));

#[cfg(feature = "parsing")]
named!(pub lifetime_def<&str, LifetimeDef>, do_parse!(
    life: lifetime >>
    bounds: opt_vec!(preceded!(
        punct!(":"),
        separated_nonempty_list!(punct!(","), lifetime)
    )) >>
    (LifetimeDef {
        lifetime: life,
        bounds: bounds,
    })
));

#[cfg(feature = "parsing")]
named!(pub bound_lifetimes<&str, Vec<LifetimeDef> >, opt_vec!(do_parse!(
    punct!("for") >>
    punct!("<") >>
    lifetimes: separated_list!(punct!(","), lifetime_def) >>
    punct!(">") >>
    (lifetimes)
)));

#[cfg(feature = "parsing")]
named!(ty_param<&str, TyParam>, do_parse!(
    ident: word >>
    bounds: opt_vec!(preceded!(
        punct!(":"),
        separated_nonempty_list!(punct!("+"), ty_param_bound)
    )) >>
    default: opt!(preceded!(
        punct!("="),
        ty
    )) >>
    (TyParam {
        ident: ident,
        bounds: bounds,
        default: default,
    })
));

#[cfg(feature = "parsing")]
named!(pub ty_param_bound<&str, TyParamBound>, alt!(
    tuple!(punct!("?"), punct!("Sized")) => { |_| TyParamBound::MaybeSized }
    |
    lifetime => { TyParamBound::Region }
    |
    poly_trait_ref => { TyParamBound::Trait }
));

#[cfg(feature = "parsing")]
named!(where_predicate<&str, WherePredicate>, alt!(
    do_parse!(
        ident: lifetime >>
        punct!(":") >>
        bounds: separated_nonempty_list!(punct!("+"), lifetime) >>
        (WherePredicate::RegionPredicate(WhereRegionPredicate {
            lifetime: ident,
            bounds: bounds,
        }))
    )
    |
    do_parse!(
        bound_lifetimes: bound_lifetimes >>
        bounded_ty: ty >>
        punct!(":") >>
        bounds: separated_nonempty_list!(punct!("+"), ty_param_bound) >>
        (WherePredicate::BoundPredicate(WhereBoundPredicate {
            bound_lifetimes: bound_lifetimes,
            bounded_ty: bounded_ty,
            bounds: bounds,
        }))
    )
));
