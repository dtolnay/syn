/// Turn a failed parse into `None` and a successful parse into `Some`.
///
/// - **Syntax:** `option!(THING)`
/// - **Output:** `Option<THING>`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::token::Bang;
///
/// named!(maybe_bang -> Option<Bang>, option!(punct!(!)));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! option {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, Some(o))),
            ::std::result::Result::Err(_) =>
                ::std::result::Result::Ok(($i, None)),
        }
    };

    ($i:expr, $f:expr) => {
        option!($i, call!($f));
    };
}

/// Turn a failed parse into an empty vector. The argument parser must itself
/// return a vector.
///
/// This is often more convenient than `option!(...)` when the argument produces
/// a vector.
///
/// - **Syntax:** `opt_vec!(THING)`
/// - **Output:** `THING`, which must be `Vec<T>`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::{Lifetime, Type};
/// use syn::delimited::Delimited;
/// use syn::token::*;
///
/// named!(bound_lifetimes -> (Vec<Lifetime>, Type), tuple!(
///     opt_vec!(do_parse!(
///         keyword!(for) >>
///         punct!(<) >>
///         lifetimes: call!(Delimited::<Lifetime, Comma>::parse_terminated) >>
///         punct!(>) >>
///         (lifetimes.into_vec())
///     )),
///     syn!(Type)
/// ));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! opt_vec {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, o)),
            ::std::result::Result::Err(_) =>
                ::std::result::Result::Ok(($i, Vec::new()))
        }
    };
}

/// Parses nothing and always succeeds.
///
/// This can be useful as a fallthrough case in `alt!`.
///
/// - **Syntax:** `epsilon!()`
/// - **Output:** `()`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Mutability;
///
/// named!(mutability -> Mutability, alt!(
///     keyword!(mut) => { Mutability::Mutable }
///     |
///     epsilon!() => { |_| Mutability::Immutable }
/// ));
///
/// # fn main() {}
#[macro_export]
macro_rules! epsilon {
    ($i:expr,) => {
        ::std::result::Result::Ok(($i, ()))
    };
}

/// Run a parser, binding the result to a name, and then evaluating an
/// expression.
///
/// Discards the result of the expression and parser.
///
/// - **Syntax:** `tap!(NAME : THING => EXPR)`
/// - **Output:** `()`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::{Expr, ExprCall};
/// use syn::token::RArrow;
///
/// named!(expr_with_arrow_call -> Expr, do_parse!(
///     mut e: syn!(Expr) >>
///     many0!(tap!(arg: tuple!(punct!(->), syn!(Expr)) => {
///         e = Expr {
///             node: ExprCall {
///                 func: Box::new(e),
///                 args: vec![arg.1].into(),
///                 paren_token: Default::default(),
///             }.into(),
///             attrs: Vec::new(),
///         };
///     })) >>
///     (e)
/// ));
///
/// # fn main() {}
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! tap {
    ($i:expr, $name:ident : $submac:ident!( $($args:tt)* ) => $e:expr) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Ok((i, o)) => {
                let $name = o;
                $e;
                ::std::result::Result::Ok((i, ()))
            }
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
        }
    };

    ($i:expr, $name:ident : $f:expr => $e:expr) => {
        tap!($i, $name: call!($f) => $e);
    };
}

/// Parse a type through the `Synom` trait.
///
/// This is a convenience macro used to invoke the `Synom::parse` method for a
/// type, you'll find this in quite a few parsers. This is also the primary way
/// to parse punctuation.
///
/// - **Syntax:** `syn!(TYPE)`
/// - **Output:** `TYPE`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Expr;
///
/// named!(expression -> Expr, syn!(Expr));
///
/// named!(expression_dot -> (Expr, Token![.]), tuple!(syn!(Expr), punct!(.)));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! syn {
    ($i:expr, $t:ty) => {
        call!($i, <$t as $crate::synom::Synom>::parse)
    };
}

/// Parse a parenthesized-surrounded subtree.
///
/// This macro will invoke a sub-parser inside of all tokens contained in
/// parenthesis. The sub-parser is required to consume all tokens within the
/// parens or else this parser will return an error.
///
/// - **Syntax:** `parens!(SUBPARSER)`
/// - **Output:** `(SUBPARSER_RET, Paren)`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Expr;
/// use syn::token::Paren;
///
/// named!(expr_paren -> (Expr, Paren), parens!(syn!(Expr)));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! parens {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        $crate::token::Paren::parse($i, |i| $submac!(i, $($args)*))
    };

    ($i:expr, $f:expr) => {
        parens!($i, call!($f));
    };
}

/// Same as the `parens` macro, but for brackets.
#[macro_export]
macro_rules! brackets {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        $crate::token::Bracket::parse($i, |i| $submac!(i, $($args)*))
    };

    ($i:expr, $f:expr) => {
        brackets!($i, call!($f));
    };
}

/// Same as the `parens` macro, but for braces.
#[macro_export]
macro_rules! braces {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        $crate::token::Brace::parse($i, |i| $submac!(i, $($args)*))
    };

    ($i:expr, $f:expr) => {
        braces!($i, call!($f));
    };
}

/// Same as the `parens` macro, but for none-delimited sequences (groups).
#[macro_export]
macro_rules! grouped {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        $crate::token::Group::parse($i, |i| $submac!(i, $($args)*))
    };

    ($i:expr, $f:expr) => {
        grouped!($i, call!($f));
    };
}
