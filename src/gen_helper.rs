#[cfg(feature = "fold")]
pub(crate) mod fold {
    pub fn vec<T, V, F>(vec: Vec<T>, fold: &mut V, mut f: F) -> Vec<T>
    where
        V: ?Sized,
        F: FnMut(&mut V, T) -> T,
    {
        vec.into_iter().map(|it| f(fold, it)).collect()
    }
}
