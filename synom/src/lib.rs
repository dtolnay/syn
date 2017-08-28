//! Adapted from [`nom`](https://github.com/Geal/nom) by removing the
//! `IPResult::Incomplete` variant which:
//!
//! - we don't need,
//! - is an unintuitive footgun when working with non-streaming use cases, and
//! - more than doubles compilation time.
//!
//! ## Whitespace handling strategy
//!
//! As (sy)nom is a parser combinator library, the parsers provided here and
//! that you implement yourself are all made up of successively more primitive
//! parsers, eventually culminating in a small number of fundamental parsers
//! that are implemented in Rust. Among these are `punct!` and `keyword!`.
//!
//! All synom fundamental parsers (those not combined out of other parsers)
//! should be written to skip over leading whitespace in their input. This way,
//! as long as every parser eventually boils down to some combination of
//! fundamental parsers, we get correct whitespace handling at all levels for
//! free.
//!
//! For our use case, this strategy is a huge improvement in usability,
//! correctness, and compile time over nom's `ws!` strategy.

extern crate proc_macro;
extern crate proc_macro2;

#[cfg(feature = "printing")]
extern crate quote;

#[doc(hidden)]
pub use proc_macro2::{TokenTree, TokenStream};

use std::convert::From;
use std::error::Error;
use std::fmt;

#[cfg(feature = "parsing")]
#[doc(hidden)]
pub mod helper;

pub mod delimited;
pub mod tokens;
pub mod span;
pub mod cursor;

pub use cursor::{SynomBuffer, Cursor};

/// The result of a parser
pub type PResult<'a, O> = Result<(Cursor<'a>, O), ParseError>;

/// An error with a default error message.
///
/// NOTE: We should provide better error messages in the future.
pub fn parse_error<O>() -> PResult<'static, O> {
    Err(ParseError(None))
}

pub trait Synom: Sized {
    fn parse(input: Cursor) -> PResult<Self>;

    fn description() -> Option<&'static str> {
        None
    }
}

#[derive(Debug)]
pub struct ParseError(Option<String>);

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.0 {
            Some(ref desc) => desc,
            None => "failed to parse",
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Display>::fmt(self.description(), f)
    }
}

impl From<proc_macro2::LexError> for ParseError {
    fn from(_: proc_macro2::LexError) -> ParseError {
        ParseError(Some("error while lexing input string".to_owned()))
    }
}

impl From<proc_macro::LexError> for ParseError {
    fn from(_: proc_macro::LexError) -> ParseError {
        ParseError(Some("error while lexing input string".to_owned()))
    }
}

impl ParseError {
    // For syn use only. Not public API.
    #[doc(hidden)]
    pub fn new<T: Into<String>>(msg: T) -> Self {
        ParseError(Some(msg.into()))
    }
}

impl Synom for TokenStream {
    fn parse(input: Cursor) -> PResult<Self> {
        Ok((Cursor::empty(), input.token_stream()))
    }
}

/// Define a function from a parser combination.
///
/// - **Syntax:** `named!(NAME -> TYPE, PARSER)` or `named!(pub NAME -> TYPE, PARSER)`
///
/// ```rust
/// # extern crate syn;
/// # #[macro_use] extern crate synom;
/// # use syn::Ty;
/// # use synom::delimited::Delimited;
/// # use synom::tokens::Comma;
/// // One or more Rust types separated by commas.
/// named!(pub comma_separated_types -> Delimited<Ty, Comma>,
///     call!(Delimited::parse_separated_nonempty)
/// );
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! named {
    ($name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name(i: $crate::Cursor) -> $crate::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    (pub $name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        pub fn $name(i: $crate::Cursor) -> $crate::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    // These two variants are for defining named parsers which have custom
    // arguments, and are called with `call!()`
    ($name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name(i: $crate::Cursor, $($params)*) -> $crate::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    (pub $name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        pub fn $name(i: $crate::Cursor, $($params)*) -> $crate::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };
}

/// Invoke the given parser function with the passed in arguments.
///
/// - **Syntax:** `call!(FUNCTION, ARGS...)`
///
///   where the signature of the function is `fn(&[U], ARGS...) -> IPResult<&[U], T>`
/// - **Output:** `T`, the result of invoking the function `FUNCTION`
#[macro_export]
macro_rules! call {
    ($i:expr, $fun:expr $(, $args:expr)*) => {
        $fun($i $(, $args)*)
    };
}

/// Transform the result of a parser by applying a function or closure.
///
/// - **Syntax:** `map!(THING, FN)`
/// - **Output:** the return type of function FN applied to THING
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Expr, ExprIf};
///
/// fn get_cond(if_: ExprIf) -> Expr {
///     *if_.cond
/// }
///
/// // Parses an `if` statement but returns the condition part only.
/// named!(if_condition -> Expr,
///     map!(syn!(ExprIf), get_cond)
/// );
///
/// // Or equivalently:
/// named!(if_condition2 -> Expr,
///     map!(syn!(ExprIf), |if_| *if_.cond)
/// );
/// #
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! map {
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, $crate::invoke($g, o))),
        }
    };

    ($i:expr, $f:expr, $g:expr) => {
        map!($i, call!($f), $g)
    };
}

// Somehow this helps with type inference in `map!`.
//
// Not public API.
#[doc(hidden)]
pub fn invoke<T, R, F: FnOnce(T) -> R>(f: F, t: T) -> R {
    f(t)
}

/// Parses successfully if the given parser fails to parse. Does not consume any
/// of the input.
///
/// - **Syntax:** `not!(THING)`
/// - **Output:** `()`
#[macro_export]
macro_rules! not {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Ok(_) => $crate::parse_error(),
            ::std::result::Result::Err(_) =>
                ::std::result::Result::Ok(($i, ())),
        }
    };
}

/// Conditionally execute the given parser.
///
/// If you are familiar with nom, this is nom's `cond_with_error` parser.
///
/// - **Syntax:** `cond!(CONDITION, THING)`
/// - **Output:** `Some(THING)` if the condition is true, else `None`
#[macro_export]
macro_rules! cond {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => {
        if $cond {
            match $submac!($i, $($args)*) {
                ::std::result::Result::Ok((i, o)) =>
                    ::std::result::Result::Ok((i, ::std::option::Option::Some(o))),
                ::std::result::Result::Err(x) => ::std::result::Result::Err(x),
            }
        } else {
            ::std::result::Result::Ok(($i, ::std::option::Option::None))
        }
    };

    ($i:expr, $cond:expr, $f:expr) => {
        cond!($i, $cond, call!($f))
    };
}

/// Fail to parse if condition is false, otherwise parse the given parser.
///
/// This is typically used inside of `option!` or `alt!`.
///
/// - **Syntax:** `cond_reduce!(CONDITION, THING)`
/// - **Output:** `THING`
#[macro_export]
macro_rules! cond_reduce {
    ($i:expr, $cond:expr, $submac:ident!( $($args:tt)* )) => {
        if $cond {
            $submac!($i, $($args)*)
        } else {
            $crate::parse_error()
        }
    };

    ($i:expr, $cond:expr, $f:expr) => {
        cond_reduce!($i, $cond, call!($f))
    };
}

/// Parse two things, returning the value of the first.
///
/// - **Syntax:** `terminated!(THING, AFTER)`
/// - **Output:** `THING`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Expr;
/// use synom::tokens::Pound;
///
/// // An expression terminated by ##.
/// named!(expr_pound_pound -> Expr,
///     terminated!(syn!(Expr), tuple!(syn!(Pound), syn!(Pound)))
/// );
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! terminated {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => {
        match tuple!($i, $submac!($($args)*), $submac2!($($args2)*)) {
            ::std::result::Result::Ok((i, (o, _))) =>
                ::std::result::Result::Ok((i, o)),
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
        }
    };

    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        terminated!($i, $submac!($($args)*), call!($g))
    };

    ($i:expr, $f:expr, $submac:ident!( $($args:tt)* )) => {
        terminated!($i, call!($f), $submac!($($args)*))
    };

    ($i:expr, $f:expr, $g:expr) => {
        terminated!($i, call!($f), call!($g))
    };
}

/// Parse zero or more values using the given parser.
///
/// - **Syntax:** `many0!(THING)`
/// - **Output:** `Vec<THING>`
///
/// You may also be looking for:
///
/// - `call!(Delimited::parse_separated)` - zero or more values with separator
/// - `call!(Delimited::parse_separated_nonempty)` - one or more values
/// - `call!(Delimited::parse_terminated)` - zero or more, allows trailing separator
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Item;
///
/// named!(items -> Vec<Item>, many0!(syn!(Item)));
///
/// # fn main() {}
#[macro_export]
macro_rules! many0 {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {{
        let ret;
        let mut res   = ::std::vec::Vec::new();
        let mut input = $i;

        loop {
            if input.eof() {
                ret = ::std::result::Result::Ok((input, res));
                break;
            }

            match $submac!(input, $($args)*) {
                ::std::result::Result::Err(_) => {
                    ret = ::std::result::Result::Ok((input, res));
                    break;
                }
                ::std::result::Result::Ok((i, o)) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if i == input {
                        ret = $crate::parse_error();
                        break;
                    }

                    res.push(o);
                    input = i;
                }
            }
        }

        ret
    }};

    ($i:expr, $f:expr) => {
        $crate::many0($i, $f)
    };
}

// Improve compile time by compiling this loop only once per type it is used
// with.
//
// Not public API.
#[doc(hidden)]
pub fn many0<'a, T>(mut input: Cursor, f: fn(Cursor) -> PResult<T>) -> PResult<Vec<T>> {
    let mut res = Vec::new();

    loop {
        if input.eof() {
            return Ok((input, res));
        }

        match f(input) {
            Err(_) => {
                return Ok((input, res));
            }
            Ok((i, o)) => {
                // loop trip must always consume (otherwise infinite loops)
                if i == input {
                    return parse_error();
                }

                res.push(o);
                input = i;
            }
        }
    }
}

/// Parse a value without consuming it from the input data.
///
/// - **Syntax:** `peek!(THING)`
/// - **Output:** `THING`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Expr, Ident};
///
/// // Parse an expression that begins with an identifier.
/// named!(ident_expr -> (Ident, Expr),
///     tuple!(peek!(syn!(Ident)), syn!(Expr))
/// );
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! peek {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Ok((_, o)) => ::std::result::Result::Ok(($i, o)),
            ::std::result::Result::Err(err) => ::std::result::Result::Err(err),
        }
    };

    ($i:expr, $f:expr) => {
        peek!($i, call!($f))
    };
}

/// Pattern-match the result of a parser to select which other parser to run.
///
/// - **Syntax:** `switch!(TARGET, PAT1 => THEN1 | PAT2 => THEN2 | ...)`
/// - **Output:** `T`, the return type of `THEN1` and `THEN2` and ...
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Ident, Ty};
/// use synom::tokens::*;
///
/// #[derive(Debug)]
/// enum UnitType {
///     Struct {
///         name: Ident,
///     },
///     Enum {
///         name: Ident,
///         variant: Ident,
///     },
/// }
///
/// // Parse a unit struct or enum: either `struct S;` or `enum E { V }`.
/// named!(unit_type -> UnitType, do_parse!(
///     which: alt!(
///         syn!(Struct) => { |_| 0 }
///         |
///         syn!(Enum) => { |_| 1 }
///     ) >>
///     id: syn!(Ident) >>
///     item: switch!(value!(which),
///         0 => map!(
///             syn!(Semi),
///             move |_| UnitType::Struct {
///                 name: id,
///             }
///         )
///         |
///         1 => map!(
///             braces!(syn!(Ident)),
///             move |(variant, _)| UnitType::Enum {
///                 name: id,
///                 variant: variant,
///             }
///         )
///     ) >>
///     (item)
/// ));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! switch {
    ($i:expr, $submac:ident!( $($args:tt)* ), $($p:pat => $subrule:ident!( $($args2:tt)* ))|* ) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) => ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) => match o {
                $(
                    $p => $subrule!(i, $($args2)*),
                )*
                _ => $crate::parse_error(),
            }
        }
    };
}


/// Produce the given value without parsing anything. Useful as an argument to
/// `switch!`.
///
/// - **Syntax:** `value!(VALUE)`
/// - **Output:** `VALUE`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Ident, Ty};
/// use synom::tokens::*;
///
/// #[derive(Debug)]
/// enum UnitType {
///     Struct {
///         name: Ident,
///     },
///     Enum {
///         name: Ident,
///         variant: Ident,
///     },
/// }
///
/// // Parse a unit struct or enum: either `struct S;` or `enum E { V }`.
/// named!(unit_type -> UnitType, do_parse!(
///     which: alt!(
///         syn!(Struct) => { |_| 0 }
///         |
///         syn!(Enum) => { |_| 1 }
///     ) >>
///     id: syn!(Ident) >>
///     item: switch!(value!(which),
///         0 => map!(
///             syn!(Semi),
///             move |_| UnitType::Struct {
///                 name: id,
///             }
///         )
///         |
///         1 => map!(
///             braces!(syn!(Ident)),
///             move |(variant, _)| UnitType::Enum {
///                 name: id,
///                 variant: variant,
///             }
///         )
///     ) >>
///     (item)
/// ));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! value {
    ($i:expr, $res:expr) => {
        ::std::result::Result::Ok(($i, $res))
    };
}

/// Run a series of parsers and produce all of the results in a tuple.
///
/// - **Syntax:** `tuple!(A, B, C, ...)`
/// - **Output:** `(A, B, C, ...)`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Ty;
///
/// named!(two_types -> (Ty, Ty), tuple!(syn!(Ty), syn!(Ty)));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! tuple {
    ($i:expr, $($rest:tt)*) => {
        tuple_parser!($i, (), $($rest)*)
    };
}

/// Internal parser, do not use directly.
#[doc(hidden)]
#[macro_export]
macro_rules! tuple_parser {
    ($i:expr, ($($parsed:tt),*), $e:ident, $($rest:tt)*) => {
        tuple_parser!($i, ($($parsed),*), call!($e), $($rest)*)
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) =>
                tuple_parser!(i, (o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt)*), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) =>
                tuple_parser!(i, ($($parsed)* , o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt),*), $e:ident) => {
        tuple_parser!($i, ($($parsed),*), call!($e))
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* )) => {
        $submac!($i, $($args)*)
    };

    ($i:expr, ($($parsed:expr),*), $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, ($($parsed),*, o))),
        }
    };

    ($i:expr, ($($parsed:expr),*)) => {
        ::std::result::Result::Ok(($i, ($($parsed),*)))
    };
}

/// Run a series of parsers, returning the result of the first one which
/// succeeds.
///
/// Optionally allows for the result to be transformed.
///
/// - **Syntax:** `alt!(THING1 | THING2 => { FUNC } | ...)`
/// - **Output:** `T`, the return type of `THING1` and `FUNC(THING2)` and ...
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Ident;
/// use synom::tokens::Bang;
///
/// named!(ident_or_bang -> Ident,
///     alt!(
///         syn!(Ident)
///         |
///         syn!(Bang) => { |_| "BANG".into() }
///     )
/// );
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! alt {
    ($i:expr, $e:ident | $($rest:tt)*) => {
        alt!($i, call!($e) | $($rest)*)
    };

    ($i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => {
        match $subrule!($i, $($args)*) {
            res @ ::std::result::Result::Ok(_) => res,
            _ => alt!($i, $($rest)*)
        }
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)+) => {
        match $subrule!($i, $($args)*) {
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, $gen(o))),
            ::std::result::Result::Err(_) => alt!($i, $($rest)*),
        }
    };

    ($i:expr, $e:ident => { $gen:expr } | $($rest:tt)*) => {
        alt!($i, call!($e) => { $gen } | $($rest)*)
    };

    ($i:expr, $e:ident => { $gen:expr }) => {
        alt!($i, call!($e) => { $gen })
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr }) => {
        match $subrule!($i, $($args)*) {
            ::std::result::Result::Ok((i, o)) =>
                ::std::result::Result::Ok((i, $gen(o))),
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
        }
    };

    ($i:expr, $e:ident) => {
        alt!($i, call!($e))
    };

    ($i:expr, $subrule:ident!( $($args:tt)*)) => {
        $subrule!($i, $($args)*)
    };
}

/// Run a series of parsers, one after another, optionally assigning the results
/// a name. Fail if any of the parsers fails.
///
/// Produces the result of evaluating the final expression in parentheses with
/// all of the previously named results bound.
///
/// - **Syntax:** `do_parse!(name: THING1 >> THING2 >> (RESULT))`
/// - **Output:** `RESULT`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
/// extern crate proc_macro2;
///
/// use syn::{Ident, TokenTree};
/// use synom::tokens::{Bang, Paren};
/// use proc_macro2::TokenStream;
///
/// // Parse a macro invocation like `stringify!($args)`.
/// named!(simple_mac -> (Ident, (TokenStream, Paren)), do_parse!(
///     name: syn!(Ident) >>
///     syn!(Bang) >>
///     body: parens!(syn!(TokenStream)) >>
///     (name, body)
/// ));
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! do_parse {
    ($i:expr, ( $($rest:expr),* )) => {
        ::std::result::Result::Ok(($i, ( $($rest),* )))
    };

    ($i:expr, $e:ident >> $($rest:tt)*) => {
        do_parse!($i, call!($e) >> $($rest)*)
    };

    ($i:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, _)) =>
                do_parse!(i, $($rest)*),
        }
    };

    ($i:expr, $field:ident : $e:ident >> $($rest:tt)*) => {
        do_parse!($i, $field: call!($e) >> $($rest)*)
    };

    ($i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) => {
                let $field = o;
                do_parse!(i, $($rest)*)
            },
        }
    };

    ($i:expr, mut $field:ident : $e:ident >> $($rest:tt)*) => {
        do_parse!($i, mut $field: call!($e) >> $($rest)*)
    };

    ($i:expr, mut $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            ::std::result::Result::Err(err) =>
                ::std::result::Result::Err(err),
            ::std::result::Result::Ok((i, o)) => {
                let mut $field = o;
                do_parse!(i, $($rest)*)
            },
        }
    };
}

#[macro_export]
macro_rules! input_end {
    ($i:expr,) => {
        $crate::input_end($i)
    };
}

// Not a public API
#[doc(hidden)]
pub fn input_end(input: Cursor) -> PResult<'static, &'static str> {
    if input.eof() {
        Ok((Cursor::empty(), ""))
    } else {
        parse_error()
    }
}
