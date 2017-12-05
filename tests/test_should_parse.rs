//! This module contains some simple tests where we can write strings which we 

extern crate syn;

macro_rules! should_parse {
    ($name:ident, { $($in:tt)* }) => {
        #[test]
        fn $name() {
            // Make sure we can parse the file!
            syn::parse_file(stringify!($($in)*)).unwrap();
        }
    }
}

should_parse!(generic_associated_type, {
    impl Foo {
        type Item<'a> = &'a i32;
        fn foo<'a>(&'a self) -> Self::Item<'a> {}
    }
});

should_parse!(const_generics_use, {
    type X = Foo<5>;
    type Y = Foo<"foo">;
    type Z = Foo<X>;
    type W = Foo<{ X + 10 }>;
});

should_parse!(trailing_plus_type, {
    type A = Box<Foo+>;
    type A = Box<Foo+'a+>;
    type A = Box<'a+Foo+>;
});
