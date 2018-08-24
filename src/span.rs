use proc_macro2::Span;

use lookahead::TokenMarker;

pub trait IntoSpans<S> {
    // Not public API.
    #[doc(hidden)]
    fn into_spans(self) -> S;
}

impl<S> IntoSpans<S> for TokenMarker {
    fn into_spans(self) -> S {
        match self {}
    }
}

impl IntoSpans<[Span; 1]> for Span {
    fn into_spans(self) -> [Span; 1] {
        [self]
    }
}

impl IntoSpans<[Span; 2]> for Span {
    fn into_spans(self) -> [Span; 2] {
        [self, self]
    }
}

impl IntoSpans<[Span; 3]> for Span {
    fn into_spans(self) -> [Span; 3] {
        [self, self, self]
    }
}

impl IntoSpans<Self> for [Span; 1] {
    fn into_spans(self) -> Self {
        self
    }
}

impl IntoSpans<Self> for [Span; 2] {
    fn into_spans(self) -> Self {
        self
    }
}

impl IntoSpans<Self> for [Span; 3] {
    fn into_spans(self) -> Self {
        self
    }
}

pub trait FromSpans: Sized {
    fn from_spans(spans: &[Span]) -> Self;
}

impl FromSpans for [Span; 1] {
    fn from_spans(spans: &[Span]) -> Self {
        [spans[0]]
    }
}

impl FromSpans for [Span; 2] {
    fn from_spans(spans: &[Span]) -> Self {
        [spans[0], spans[1]]
    }
}

impl FromSpans for [Span; 3] {
    fn from_spans(spans: &[Span]) -> Self {
        [spans[0], spans[1], spans[2]]
    }
}
