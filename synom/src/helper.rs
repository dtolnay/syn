use proc_macro2::{TokenTree, TokenKind};

use IResult;
use span::Span;

/// Parse a keyword like "fn" or "struct".
///
/// See also `punct!` for parsing punctuation, which are subtly different from
/// keywords.
///
/// - **Syntax:** `keyword!("...")`
/// - **Output:** `&str`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use synom::IResult;
///
/// // Parse zero or more "bang" keywords.
/// named!(many_bangs -> Vec<&str>,
///     terminated!(
///         many0!(keyword!("bang")),
///         punct!(";")
///     )
/// );
///
/// fn main() {
///     let input = "bang bang bang;";
///     let parsed = many_bangs(input).expect("bangs");
///     assert_eq!(parsed, ["bang", "bang", "bang"]);
///
///     let input = "bangbang;";
///     let err = many_bangs(input);
///     assert_eq!(err, IResult::Error);
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
pub fn keyword<'a>(input: &'a [TokenTree], token: &'static str) -> IResult<&'a [TokenTree], Span> {
    match input.first() {
        Some(&TokenTree{ kind: TokenKind::Word(ref symbol), span }) => {
            if symbol.as_str() == token {
                IResult::Done(&input[1..], Span(span))
            } else {
                IResult::Error
            }
        }
        _ => IResult::Error,
    }
}

/// Turn a failed parse into `None` and a successful parse into `Some`.
///
/// - **Syntax:** `option!(THING)`
/// - **Output:** `Option<THING>`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// named!(maybe_bang -> Option<&str>, option!(punct!("!")));
///
/// fn main() {
///     let input = "!";
///     let parsed = maybe_bang(input).expect("maybe bang");
///     assert_eq!(parsed, Some("!"));
///
///     let input = "";
///     let parsed = maybe_bang(input).expect("maybe bang");
///     assert_eq!(parsed, None);
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
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::{Lifetime, Ty};
/// use syn::parse::{lifetime, ty};
///
/// named!(bound_lifetimes -> (Vec<Lifetime>, Ty), tuple!(
///     opt_vec!(do_parse!(
///         keyword!("for") >>
///         punct!("<") >>
///         lifetimes: terminated_list!(punct!(","), lifetime) >>
///         punct!(">") >>
///         (lifetimes.into_vec())
///     )),
///     ty
/// ));
///
/// fn main() {
///     let input = "for<'a, 'b> fn(&'a A) -> &'b B";
///     let parsed = bound_lifetimes(input).expect("bound lifetimes");
///     assert_eq!(parsed.0, [Lifetime::new("'a"), Lifetime::new("'b")]);
///     println!("{:?}", parsed);
///
///     let input = "From<String>";
///     let parsed = bound_lifetimes(input).expect("bound lifetimes");
///     assert!(parsed.0.is_empty());
///     println!("{:?}", parsed);
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
/// This can be useful as a fallthrough case in `alt!`.
///
/// - **Syntax:** `epsilon!()`
/// - **Output:** `()`
///
/// ```rust
/// extern crate syn;
/// #[macro_use] extern crate synom;
///
/// use syn::Mutability;
///
/// named!(mutability -> Mutability, alt!(
///     keyword!("mut") => { |_| Mutability::Mutable(Default::default()) }
///     |
///     epsilon!() => { |_| Mutability::Immutable }
/// ));
///
/// fn main() {
///     let input = "mut";
///     let parsed = mutability(input).expect("mutability");
///     assert_eq!(parsed, Mutability::Mutable(Default::default()));
///
///     let input = "";
///     let parsed = mutability(input).expect("mutability");
///     assert_eq!(parsed, Mutability::Immutable);
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
/// use syn::{Expr, ExprCall};
/// use syn::parse::expr;
///
/// named!(expr_with_arrow_call -> Expr, do_parse!(
///     mut e: expr >>
///     many0!(tap!(arg: tuple!(punct!("=>"), expr) => {
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
/// fn main() {
///     let input = "something => argument1 => argument2";
///
///     let parsed = expr_with_arrow_call(input).expect("expr with arrow call");
///
///     println!("{:?}", parsed);
/// }
/// ```
#[doc(hidden)]
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
