use buffer::Cursor;
use token::Token;

pub trait Keyword {
    fn ident() -> &'static str;

    fn display() -> &'static str;
}

impl<K: Keyword> Token for K {
    fn peek(cursor: Cursor) -> bool {
        if let Some((ident, _rest)) = cursor.ident() {
            ident == K::ident()
        } else {
            false
        }
    }

    fn display() -> &'static str {
        K::display()
    }
}

#[macro_export]
macro_rules! custom_keyword {
    ($ident:ident) => {
        custom_keyword_internal!({ pub(in self) } $ident);
    };
    (pub $ident:ident) => {
        custom_keyword_internal!({ pub } $ident);
    };
    (pub(crate) $ident:ident) => {
        custom_keyword_internal!({ pub(crate) } $ident);
    };
    (pub(super) $ident:ident) => {
        custom_keyword_internal!({ pub(super) } $ident);
    };
    (pub(self) $ident:ident) => {
        custom_keyword_internal!({ pub(self) } $ident);
    };
    (pub(in $path:path) $ident:ident) => {
        custom_keyword_internal!({ pub(in $path) } $ident);
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! custom_keyword_internal {
    ({ $($vis:tt)* } $ident:ident) => {
        $($vis)* struct $ident {
            inner: $crate::Ident
        }

        impl $crate::parse::Keyword for $ident {
            fn ident() -> &'static str {
                stringify!($ident)
            }

            fn display() -> &'static str {
                concat!("`", stringify!($ident), "`")
            }
        }

        impl $crate::parse::Parse for $ident {
            fn parse(input: $crate::parse::ParseStream) -> $crate::parse::Result<$ident> {
                input.step(|cursor| {
                    if let Some((ident, rest)) = cursor.ident() {
                        if ident == stringify!($ident) {
                            return Ok(($ident { inner: ident }, rest));
                        }
                    }
                    Err(cursor.error(concat!("expected `", stringify!($ident), "`")))
                })
            }
        }

        $($vis)* fn $ident(marker: $crate::parse::TokenMarker) -> $ident {
            match marker {}
        }
    }
}
