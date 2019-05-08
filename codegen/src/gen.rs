use syn_codegen as types;

pub const TERMINAL_TYPES: &[&str] = &["Span", "Ident"];

pub fn traverse<S, F>(defs: &types::Definitions, node: F) -> S
where
    S: Default,
    F: Fn(&mut S, &types::Node, &types::Definitions),
{
    let mut state = S::default();
    for s in &defs.types {
        node(&mut state, s, defs);
    }
    for tt in TERMINAL_TYPES {
        let s = types::Node {
            ident: tt.to_string(),
            features: types::Features::default(),
            data: types::Data::Private,
        };
        node(&mut state, &s, defs);
    }
    state
}
