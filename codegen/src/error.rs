use std::fmt::{self, Display};
use std::io;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Json(serde_json::Error),
    Rustfmt(rustfmt::ErrorKind),
    Syn(syn::Error),
    Toml(toml::de::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match self {
            Io(e) => write!(f, "{}", e),
            Json(e) => write!(f, "{}", e),
            Rustfmt(e) => write!(f, "{}", e),
            Syn(e) => write!(f, "{}", e),
            Toml(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

impl From<rustfmt::ErrorKind> for Error {
    fn from(e: rustfmt::ErrorKind) -> Self {
        Error::Rustfmt(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Json(e)
    }
}

impl From<syn::Error> for Error {
    fn from(e: syn::Error) -> Self {
        Error::Syn(e)
    }
}

impl From<toml::de::Error> for Error {
    fn from(e: toml::de::Error) -> Self {
        Error::Toml(e)
    }
}
