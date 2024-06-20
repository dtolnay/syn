#[cfg(feature = "parsing")]
pub(crate) mod lookahead {
    #[cfg(not(docsrs))]
    pub trait Sealed: Copy {}

    #[cfg(docsrs)]
    pub trait Sealed {}

    #[cfg(docsrs)]
    impl<T> Sealed for T {}
}
