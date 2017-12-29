#[cfg(feature = "fold")]
pub mod fold {
    use delimited::Delimited;

    pub trait FoldHelper {
        type Item;
        fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item;
    }

    impl<T> FoldHelper for Vec<T> {
        type Item = T;
        fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {
            self.into_iter().map(f).collect()
        }
    }

    impl<T, U> FoldHelper for Delimited<T, U> {
        type Item = T;
        fn lift<F>(self, mut f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {
            self.into_iter().map(|elem| {
                let (t, u) = elem.into_tuple();
                (f(t), u)
            }).collect::<Vec<(T, Option<U>)>>().into()
        }
    }
}
