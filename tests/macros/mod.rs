extern crate proc_macro2;

use syn;
use syn::parse::{Parse, Result};

#[macro_export]
macro_rules! errorf {
    ($($tt:tt)*) => {{
        use ::std::io::Write;
        let stderr = ::std::io::stderr();
        write!(stderr.lock(), $($tt)*).unwrap();
    }};
}

#[macro_export]
macro_rules! punctuated {
    ($($e:expr,)+) => {{
        let mut seq = ::syn::punctuated::Punctuated::new();
        $(
            seq.push($e);
        )+
        seq
    }};

    ($($e:expr),+) => {
        punctuated!($($e,)+)
    };
}

#[macro_export]
macro_rules! snapshot {
    ($($args:tt)*) => {
        snapshot_impl!(() $($args)*)
    };
}

#[macro_export]
macro_rules! snapshot_impl {
    (($($expr:tt)*) as $t:ty) => {{
        let syntax_tree = ::macros::Tokens::parse::<$t>($($expr)*).unwrap();
        insta::assert_debug_snapshot_matches!(syntax_tree);
        syntax_tree
    }};
    (($($expr:tt)*)) => {{
        let syntax_tree = $($expr)*;
        insta::assert_debug_snapshot_matches!(syntax_tree);
        syntax_tree
    }};
    (($($expr:tt)*) $next:tt $($rest:tt)*) => {
        snapshot_impl!(($($expr)* $next) $($rest)*)
    };
}

pub trait Tokens {
    fn parse<T: Parse>(self) -> Result<T>;
}

impl<'a> Tokens for &'a str {
    fn parse<T: Parse>(self) -> Result<T> {
        syn::parse_str(self)
    }
}

impl Tokens for proc_macro2::TokenStream {
    fn parse<T: Parse>(self) -> Result<T> {
        syn::parse2(self)
    }
}
