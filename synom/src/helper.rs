use IResult;
use space::{skip_whitespace, word_break};

/// Parse a piece of punctuation, skipping whitespace before it.
///
/// - **Syntax:** `punct!("...")`
/// - **Output:** `&str`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// named!(bang -> &str, punct!("!"));
///
/// fn main() {
///     let input = "   !";
///     bang(input).expect("bang");
/// }
/// ```
#[macro_export]
macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::helper::punct($i, $punct)
    };
}

// Not public API.
#[doc(hidden)]
pub fn punct<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    let input = skip_whitespace(input);
    if input.starts_with(token) {
        IResult::Done(&input[token.len()..], token)
    } else {
        IResult::Error
    }
}

/// Parse a keyword. The word must make up a complete identifier.
///
/// - **Syntax:** `keyword!("...")`
/// - **Output:** `&str`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// named!(apple -> &str, keyword!("apple"));
///
/// fn main() {
///     let input = "   apple";
///     apple(input).expect("apple");
///     let input = "apples";
///     assert_eq!(apple(input), synom::IResult::Error);
/// }
/// ```
#[macro_export]
macro_rules! keyword {
    ($i:expr, $keyword:expr) => {
        $crate::helper::keyword($i, $keyword)
    };
}

// Not public API.
#[doc(hidden)]
pub fn keyword<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    match punct(input, token) {
        IResult::Done(rest, _) => {
            match word_break(rest) {
                IResult::Done(_, _) => IResult::Done(rest, token),
                IResult::Error => IResult::Error,
            }
        }
        IResult::Error => IResult::Error,
    }
}

/// Try to run the parser and wrap it in an `Option`, if it fails, succeed but
/// produce a `None`.
///
/// - **Syntax:** `option!(THING)`
/// - **Output:** `THING`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// named!(maybe_bang -> Option<&str>, option!(punct!("!")));
///
/// fn main() {
///     let input = "   !";
///     assert_eq!(maybe_bang(input).expect("maybe bang"), Some("!"));
///     let input = "";
///     assert_eq!(maybe_bang(input).expect("maybe bang"), None);
/// }
/// ```
#[macro_export]
macro_rules! option {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::IResult::Done(i, o) => $crate::IResult::Done(i, Some(o)),
            $crate::IResult::Error => $crate::IResult::Done($i, None),
        }
    };

    ($i:expr, $f:expr) => {
        option!($i, call!($f));
    };
}

/// Try to run the parser, if it fails, succeed and produce an empty Vec.
///
/// The argument parser must be a Vec.
///
/// - **Syntax:** `opt_vec!(THING)`
/// - **Output:** `THING`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Expr;
/// use syn::parse::expr;
///
/// named!(opt_expr_list -> Vec<Expr>, opt_vec!(
///     separated_list!(punct!(","), expr)));
///
/// fn main() {
///     let input = "a, 1 + 1, Object { construct: ion }";
///     let result = opt_expr_list(input).expect("opt expr list");
///     assert_eq!(result.len(), 3);
///
///     let input = "";
///     let result = opt_expr_list(input).expect("opt expr list");
///     assert_eq!(result.len(), 0);
/// }
/// ```
#[macro_export]
macro_rules! opt_vec {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::IResult::Done(i, o) => $crate::IResult::Done(i, o),
            $crate::IResult::Error => $crate::IResult::Done($i, Vec::new()),
        }
    };
}

/// Parses nothing and always succeeds.
///
/// - **Syntax:** `epsilon!()`
/// - **Output:** `()`
///
/// ```rust
/// #[macro_use] extern crate synom;
///
/// named!(epsi -> (), epsilon!());
///
/// fn main() {
///     let input = "";
///     assert_eq!(epsi(input).expect("maybe bang"), ());
/// }
/// ```
#[macro_export]
macro_rules! epsilon {
    ($i:expr,) => {
        $crate::IResult::Done($i, ())
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
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Expr, ExprKind};
/// use syn::parse::expr;
///
/// named!(pub expr_with_arrow_call -> Expr, do_parse!(
///     mut e: expr >>
///     many0!(tap!(arg: tuple!(punct!("=>"), expr) => {
///         e = Expr {
///             node: ExprKind::Call(Box::new(e), vec![arg.1]),
///             attrs: Vec::new(),
///         };
///     })) >>
///     (e)
/// ));
///
/// fn main() {
///     let input = "something => argument1 => argument2";
///
///     let result = expr_with_arrow_call(input).expect("expr with arrow call");
///
///     println!("result = {:?}", result);
/// }
/// ```
#[macro_export]
macro_rules! tap {
    ($i:expr, $name:ident : $submac:ident!( $($args:tt)* ) => $e:expr) => {
        match $submac!($i, $($args)*) {
            $crate::IResult::Done(i, o) => {
                let $name = o;
                $e;
                $crate::IResult::Done(i, ())
            }
            $crate::IResult::Error => $crate::IResult::Error,
        }
    };

    ($i:expr, $name:ident : $f:expr => $e:expr) => {
        tap!($i, $name: call!($f) => $e);
    };
}

/// Parses a series of things, separated by the given punctuation. Does not
/// allow for a trailing seperator.
///
/// The implementation requires that the first parameter is a `punct!` macro,
/// and the second is a named parser.
///
/// - **Syntax:** `separated_list!(punct!("..."), THING)`
/// - **Output:** `Vec<THING>`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Expr;
/// use syn::parse::expr;
///
/// named!(pub expr_list -> Vec<Expr>,
///     separated_list!(punct!(","), expr));
///
/// fn main() {
///     let input = "1 + 1, things, Construct { this: thing }";
///
///     let result = expr_list(input).expect("expr list");
///     assert_eq!(result.len(), 3);
/// }
/// ```
#[macro_export]
macro_rules! separated_list {
    ($i:expr, punct!($sep:expr), $f:expr) => {
        $crate::helper::separated_list($i, $sep, $f, false)
    };
}

/// Parses a series of things, separated by the given punctuation. Allows for
/// a trailing seperator.
///
/// The implementation requires that the first parameter is a `punct!` macro,
/// and the second is a named parser.
///
/// - **Syntax:** `terminated_list!(punct!("..."), THING)`
/// - **Output:** `Vec<THING>`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Expr;
/// use syn::parse::expr;
///
/// named!(pub expr_list -> Vec<Expr>,
///     terminated_list!(punct!(","), expr));
///
/// fn main() {
///     let input = "1 + 1, things, Construct { this: thing },";
///
///     let result = expr_list(input).expect("expr list");
///     assert_eq!(result.len(), 3);
/// }
/// ```
#[macro_export]
macro_rules! terminated_list {
    ($i:expr, punct!($sep:expr), $f:expr) => {
        $crate::helper::separated_list($i, $sep, $f, true)
    };
}

// Not public API.
#[doc(hidden)]
pub fn separated_list<'a, T>(mut input: &'a str,
                             sep: &'static str,
                             f: fn(&'a str) -> IResult<&'a str, T>,
                             terminated: bool)
                             -> IResult<&'a str, Vec<T>> {
    let mut res = Vec::new();

    // get the first element
    match f(input) {
        IResult::Error => IResult::Done(input, Vec::new()),
        IResult::Done(i, o) => {
            if i.len() == input.len() {
                IResult::Error
            } else {
                res.push(o);
                input = i;

                // get the separator first
                while let IResult::Done(i2, _) = punct(input, sep) {
                    if i2.len() == input.len() {
                        break;
                    }

                    // get the element next
                    if let IResult::Done(i3, o3) = f(i2) {
                        if i3.len() == i2.len() {
                            break;
                        }
                        res.push(o3);
                        input = i3;
                    } else {
                        break;
                    }
                }
                if terminated {
                    if let IResult::Done(after, _) = punct(input, sep) {
                        input = after;
                    }
                }
                IResult::Done(input, res)
            }
        }
    }
}
