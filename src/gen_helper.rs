#[cfg(feature = "fold")]
pub(crate) mod fold {
    use crate::fold::Fold;

    pub(crate) trait FoldHelper {
        type Item;
        fn lift<V, F>(self, fold: &mut V, f: F) -> Self
        where
            V: Fold + ?Sized,
            F: FnMut(&mut V, Self::Item) -> Self::Item;
    }

    impl<T> FoldHelper for Vec<T> {
        type Item = T;
        fn lift<V, F>(self, fold: &mut V, mut f: F) -> Self
        where
            V: Fold + ?Sized,
            F: FnMut(&mut V, Self::Item) -> Self::Item,
        {
            self.into_iter().map(|it| f(fold, it)).collect()
        }
    }
}
