#[cfg(feature = "parsing")]
pub mod lookahead {
    pub trait Sealed: Copy {}
}

#[cfg(feature = "parsing")]
pub mod discouraged {
    pub trait Sealed {}
}
