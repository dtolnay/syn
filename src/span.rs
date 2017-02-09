use std::usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn extend(self, other: Span) -> Span {
        Span {
            lo: self.lo,
            hi: other.hi,
        }
    }
}

pub const EMPTY_SPAN: Span = Span { lo: 0, hi: 0 };

pub const DUMMY_SPAN: Span = Span { lo: usize::MAX, hi: usize::MAX };

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

#[cfg(feature = "parsing")]
#[macro_use]
mod parsing {
    #[macro_export]
    macro_rules! spanned {
        ($i:expr, $submac:ident!( $($args:tt)* )) => {
            match $submac!($i, $($args)*) {
                ::synom::IResult::Done(rest, node) => {
                    let span = $crate::Span {
                        lo: ::synom::space::skip_whitespace($i).idx(),
                        hi: rest.idx()
                    };
                    ::synom::IResult::Done(rest, $crate::Spanned { node: node, span: span })
                }
                ::synom::IResult::Error => ::synom::IResult::Error,
            }
        };
        ($i:expr, $f:expr) => {
            spanned!($i, call!($f))
        };
    }

    #[macro_export]
    macro_rules! add_span {
        ($i:expr, $submac:ident!( $($args:tt)* )) => {
            match $submac!($i, $($args)*) {
                ::synom::IResult::Done(rest, mut val) => {
                    assert_eq!(val.span, $crate::DUMMY_SPAN);
                    val.span = $crate::Span {
                        lo: ::synom::space::skip_whitespace($i).idx(),
                        hi: rest.idx()
                    };
                    ::synom::IResult::Done(rest, val)
                }
                ::synom::IResult::Error => ::synom::IResult::Error,
            }
        };
        ($i:expr, $f:expr) => {
            spanned!($i, call!($f))
        };
    }
}
