extern crate syn;

use syn::{
    parse::{Parse, ParseStream},
    parse::discouraged::Speculative,
    Result,
};

#[test]
#[should_panic(expected = "Fork was not derived from the advancing parse stream")]
fn smuggled_speculative_cursor() {
    struct Smuggled(ParseStream<'static>);
    impl Parse for Smuggled {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Smuggled(unsafe { std::mem::transmute_copy(input) }))
        }
    }

    struct BreakRules;
    impl Parse for BreakRules {
        fn parse(input: ParseStream) -> Result<Self> {
            let Smuggled(fork) = syn::parse_str("").unwrap();
            input.advance_to(fork);
            Ok(Self)
        }
    }

    syn::parse_str::<BreakRules>("").unwrap();
}
