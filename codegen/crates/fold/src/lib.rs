#![cfg_attr(docsrs, feature(doc_cfg))]

#[rustfmt::skip]
mod fold;

pub use fold::*;
use syn::*;

#[cfg(any(feature = "full", feature = "derive"))]
mod punctuated {
    use syn::punctuated::{Pair, Punctuated};

    pub(crate) fn fold<T, P, V, F>(
        punctuated: Punctuated<T, P>,
        fold: &mut V,
        mut f: F,
    ) -> Punctuated<T, P>
    where
        V: ?Sized,
        F: FnMut(&mut V, T) -> T,
    {
        punctuated
            .into_pairs()
            .map(|pair| match pair {
                Pair::Punctuated(t, p) => Pair::Punctuated(f(fold, t), p),
                Pair::End(t) => Pair::End(f(fold, t)),
            })
            .collect()
    }
}
