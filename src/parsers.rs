use cursor::Cursor;
use parse_error;
use synom::PResult;

/// Define a function from a parser combination.
///
/// - **Syntax:** `named!(NAME -> TYPE, PARSER)` or `named!(pub NAME -> TYPE, PARSER)`
///
/// ```rust
/// # #[macro_use]
/// # extern crate syn;
/// #
/// # use syn::Type;
/// # use syn::delimited::Delimited;
/// #
/// // One or more Rust types separated by commas.
/// named!(pub comma_separated_types -> Delimited<Type, Token![,]>,
///     call!(Delimited::parse_separated_nonempty)
/// );
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! named {
    ($name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name(i: $crate::synom::Cursor) -> $crate::synom::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    (pub $name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        pub fn $name(i: $crate::synom::Cursor) -> $crate::synom::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    // These two variants are for defining named parsers which have custom
    // arguments, and are called with `call!()`
    ($name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name(i: $crate::synom::Cursor, $($params)*) -> $crate::synom::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };

    (pub $name:ident($($params:tt)*) -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        pub fn $name(i: $crate::synom::Cursor, $($params)*) -> $crate::synom::PResult<$o> {
            $submac!(i, $($args)*)
        }
    };
}

#[cfg(all(feature = "verbose-trace", not(feature = "all-features")))]
#[macro_export]
macro_rules! call {
    ($i:expr, $fun:expr $(, $args:expr)*) => {
        {
            let i = $i;
            eprintln!(concat!(" -> ", stringify!($fun), " @ {:?}"), i);
            let r = $fun(i $(, $args)*);
            match r {
                Ok((i, _)) => eprintln!(concat!("OK  ", stringify!($fun), " @ {:?}"), i),
                Err(_) => eprintln!(concat!("ERR ", stringify!($fun), " @ {:?}"), i),
            }
            r
        }
    };
}

/// Invoke the given parser function with the passed in arguments.
///
/// - **Syntax:** `call!(FUNCTION, ARGS...)`
///
///   where the signature of the function is `fn(&[U], ARGS...) -> IPResult<&[U], T>`
/// - **Output:** `T`, the result of invoking the function `FUNCTION`
#[cfg(any(not(feature = "verbose-trace"), feature = "all-features"))]
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
/// #[macro_use]
/// extern crate syn;
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
                ::std::result::Result::Ok((i, $crate::parsers::invoke($g, o))),
        }
    };

    ($i:expr, $f:expr, $g:expr) => {
        map!($i, call!($f), $g)
    };
}

// Somehow this helps with type inference in `map!` and `alt!`.
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
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Expr;
///
/// // An expression terminated by ##.
/// named!(expr_pound_pound -> Expr,
///     terminated!(syn!(Expr), tuple!(punct!(#), punct!(#)))
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
/// #[macro_use]
/// extern crate syn;
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
        $crate::parsers::many0($i, $f)
    };
}

// Improve compile time by compiling this loop only once per type it is used
// with.
//
// Not public API.
#[doc(hidden)]
pub fn many0<T>(mut input: Cursor, f: fn(Cursor) -> PResult<T>) -> PResult<Vec<T>> {
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
/// #[macro_use]
/// extern crate syn;
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
/// #[macro_use]
/// extern crate syn;
///
/// use syn::{Ident, Type};
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
///         keyword!(struct) => { |_| 0 }
///         |
///         keyword!(enum) => { |_| 1 }
///     ) >>
///     id: syn!(Ident) >>
///     item: switch!(value!(which),
///         0 => map!(
///             punct!(;),
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
///         |
///         _ => reject!()
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
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Ident;
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
///     is_struct: alt!(
///         keyword!(struct) => { |_| true }
///         |
///         keyword!(enum) => { |_| false }
///     ) >>
///     id: syn!(Ident) >>
///     item: switch!(value!(is_struct),
///         true => map!(
///             punct!(;),
///             move |_| UnitType::Struct {
///                 name: id,
///             }
///         )
///         |
///         false => map!(
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

/// Unconditionally fail to parse anything. This may be useful in ignoring some
/// arms of a `switch!` parser.
///
/// - **Syntax:** `reject!()`
/// - **Output:** never succeeds
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Item;
///
/// // Parse any item, except for a module.
/// named!(almost_any_item -> Item,
///     switch!(syn!(Item),
///         Item::Mod(_) => reject!()
///         |
///         ok => value!(ok)
///     )
/// );
///
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! reject {
    ($i:expr,) => {{
        let _ = $i;
        $crate::parse_error()
    }}
}

/// Run a series of parsers and produce all of the results in a tuple.
///
/// - **Syntax:** `tuple!(A, B, C, ...)`
/// - **Output:** `(A, B, C, ...)`
///
/// ```rust
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Type;
///
/// named!(two_types -> (Type, Type), tuple!(syn!(Type), syn!(Type)));
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
/// #[macro_use]
/// extern crate syn;
///
/// use syn::Ident;
///
/// named!(ident_or_bang -> Ident,
///     alt!(
///         syn!(Ident)
///         |
///         punct!(!) => { |_| "BANG".into() }
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
                ::std::result::Result::Ok((i, $crate::parsers::invoke($gen, o))),
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
                ::std::result::Result::Ok((i, $crate::parsers::invoke($gen, o))),
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
/// #[macro_use]
/// extern crate syn;
/// extern crate proc_macro2;
///
/// use syn::Ident;
/// use syn::token::Paren;
/// use proc_macro2::TokenStream;
///
/// // Parse a macro invocation like `stringify!($args)`.
/// named!(simple_mac -> (Ident, (TokenStream, Paren)), do_parse!(
///     name: syn!(Ident) >>
///     punct!(!) >>
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
        $crate::parsers::input_end($i)
    };
}

// Not a public API
#[doc(hidden)]
pub fn input_end(input: Cursor) -> PResult<'static, ()> {
    if input.eof() {
        Ok((Cursor::empty(), ()))
    } else {
        parse_error()
    }
}

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
/// enum Mutability {
///     Mutable(Token![mut]),
///     Immutable,
/// }
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
///         e = Expr::Call(ExprCall {
///             attrs: Vec::new(),
///             func: Box::new(e),
///             args: vec![arg.1].into(),
///             paren_token: Default::default(),
///         });
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
