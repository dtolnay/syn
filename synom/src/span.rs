use std::hash::{Hash, Hasher};
use std::fmt::{self, Debug};

use proc_macro2;

#[derive(Clone, Copy, Default)]
pub struct Span(pub proc_macro2::Span);

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for Span {
    fn eq(&self, _other: &Span) -> bool {
        true
    }
}

impl Eq for Span {}

impl Hash for Span {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {}
}

pub trait FromSpan {
    fn from_span(span: Span) -> Self;
}

impl FromSpan for Span {
    fn from_span(span: Span) -> Self { span }
}

impl FromSpan for proc_macro2::Span {
    fn from_span(span: Span) -> Self { span.0 }
}

macro_rules! impl_array {
    ($n:expr) => {
        impl<T: FromSpan + Copy> FromSpan for [T; $n] {
            fn from_span(span: Span) -> Self {
                let e = FromSpan::from_span(span);
                [e; $n]
            }
        }
    };
    ($n:expr, $($rest:tt)*) => {
        impl_array!($n);
        impl_array!($($rest)*);
    };
}

impl_array!(1, 2, 3, 4);
