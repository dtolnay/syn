use inflections::Inflect;
use proc_macro2::{Ident, Span, TokenStream};
use syn_codegen::{Data, Features, Definitions, Node};

pub const TERMINAL_TYPES: &[&str] = &["Span", "Ident"];

pub fn under_name(name: &str) -> Ident {
    Ident::new(&name.to_snake_case(), Span::call_site())
}

pub fn traverse(
    defs: &Definitions,
    node: fn(&mut TokenStream, &mut TokenStream, &Node, &Definitions),
) -> (TokenStream, TokenStream) {
    let mut traits = TokenStream::new();
    let mut impls = TokenStream::new();
    for s in &defs.types {
        node(&mut traits, &mut impls, s, defs);
    }
    for tt in TERMINAL_TYPES {
        let s = Node {
            ident: tt.to_string(),
            features: Features::default(),
            data: Data::Private,
        };
        node(&mut traits, &mut impls, &s, defs);
    }
    (traits, impls)
}
