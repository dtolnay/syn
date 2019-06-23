#[macro_use]
extern crate syn;

use syn::{
    parse::discouraged::Speculative,
    parse::{Parse, ParseStream},
    Result,
};

#[test]
#[should_panic(expected = "Fork was not derived from the advancing parse stream")]
fn smuggled_speculative_cursor_between_sources() {
    // don't do this ever this is very unsafe and fails anyway
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

#[test]
#[should_panic(expected = "Fork was not derived from the advancing parse stream")]
fn smuggled_speculative_cursor_between_brackets() {
    struct BreakRules;
    impl Parse for BreakRules {
        fn parse(input: ParseStream) -> Result<Self> {
            let a;
            let b;
            parenthesized!(a in input);
            parenthesized!(b in input);
            a.advance_to(&b);
            Ok(Self)
        }
    }

    syn::parse_str::<BreakRules>("()()").unwrap();
}

#[test]
#[should_panic(expected = "Fork was not derived from the advancing parse stream")]
fn smuggled_speculative_cursor_into_brackets() {
    struct BreakRules;
    impl Parse for BreakRules {
        fn parse(input: ParseStream) -> Result<Self> {
            let a;
            parenthesized!(a in input);
            input.advance_to(&a);
            Ok(Self)
        }
    }

    syn::parse_str::<BreakRules>("()").unwrap();
}
