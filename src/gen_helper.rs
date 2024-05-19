#[cfg(feature = "fold")]
pub(crate) mod fold {
    use crate::punctuated::{Pair, Punctuated};

    pub(crate) trait FoldHelper {
        type Item;
        fn lift<F>(self, f: F) -> Self
        where
            F: FnMut(Self::Item) -> Self::Item;
    }

    impl<T> FoldHelper for Vec<T> {
        type Item = T;
        fn lift<F>(self, f: F) -> Self
        where
            F: FnMut(Self::Item) -> Self::Item,
        {
            self.into_iter().map(f).collect()
        }
    }

    impl<T, P> FoldHelper for Punctuated<T, P> {
        type Item = T;
        fn lift<F>(self, mut f: F) -> Self
        where
            F: FnMut(Self::Item) -> Self::Item,
        {
            self.into_pairs()
                .map(|pair| match pair {
                    Pair::Punctuated(t, p) => Pair::Punctuated(f(t), p),
                    Pair::End(t) => Pair::End(f(t)),
                })
                .collect()
        }
    }
}
