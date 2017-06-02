use std::hash::{Hash, Hasher};

use proc_macro2;

#[derive(Clone, Copy, Default, Debug)]
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
