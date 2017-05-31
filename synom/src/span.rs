use std::hash::{Hash, Hasher};
use std::fmt;

use proc_macro2;

#[derive(Clone, Copy, Default)]
pub struct Span(pub proc_macro2::Span);

impl PartialEq for Span {
    fn eq(&self, _other: &Span) -> bool {
        true
    }
}

impl Eq for Span {}

impl Hash for Span {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
         .finish()
    }
}
