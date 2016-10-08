Nom parser for Rust items
=========================

[![Build Status](https://api.travis-ci.org/dtolnay/syn.svg?branch=master)](https://travis-ci.org/dtolnay/syn)
[![Latest Version](https://img.shields.io/crates/v/syn.svg)](https://crates.io/crates/syn)
[![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://dtolnay.github.io/syn/syn/)

Parse Rust structs and enums without a Syntex dependency, intended for use with
[Macros 1.1](https://github.com/rust-lang/rfcs/blob/master/text/1681-macros-1.1.md).

Designed for fast compile time.

- Compile time for `syn` (from scratch including all dependencies): **4 seconds**
- Compile time for the `syntex`/`quasi`/`aster` stack: **60+ seconds**

## Usage with Macros 1.1

```toml
[dependencies]
syn = "0.9"
quote = "0.3"

[lib]
rustc-macro = true
```

```rust
#![feature(rustc_macro, rustc_macro_lib)]

extern crate rustc_macro;
use rustc_macro::TokenStream;

extern crate syn;

#[macro_use]
extern crate quote;

#[rustc_macro_derive(MyMacro)]
pub fn my_macro(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    // Parse the string representation to an AST
    let ast = syn::parse_macro_input(&source).unwrap();

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        // ...
    };

    // Parse back to a token stream and return it
    expanded.to_string().parse().unwrap()
}
```

## Complete example

Suppose we have the following simple trait which returns the number of fields in
a struct:

```rust
trait NumFields {
    fn num_fields() -> usize;
}
```

A complete Macros 1.1 implementation of `#[derive(NumFields)]` based on `syn`
and [`quote`](https://github.com/dtolnay/quote) looks like this:

```rust
#![feature(rustc_macro, rustc_macro_lib)]

extern crate rustc_macro;
use rustc_macro::TokenStream;

extern crate syn;

#[macro_use]
extern crate quote;

#[rustc_macro_derive(NumFields)]
pub fn num_fields(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    // Parse the string representation to an AST
    let ast = syn::parse_macro_input(&source).unwrap();

    // Build the output
    let expanded = expand_num_fields(ast);

    // Parse back to a token stream and return it
    expanded.to_string().parse().unwrap()
}

fn expand_num_fields(ast: syn::MacroInput) -> quote::Tokens {
    let n = match ast.body {
        syn::Body::Struct(ref data) => data.fields().len(),
        syn::Body::Enum(_) => panic!("#[derive(NumFields)] can only be used with structs"),
    };

    // Used in the quasi-quotation below as `#name`
    let name = &ast.ident;

    // Helper is provided for handling complex generic types correctly and effortlessly
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    quote! {
        // Preserve the input struct unmodified
        #ast

        // The generated impl
        impl #impl_generics ::mycrate::NumFields for #name #ty_generics #where_clause {
            fn num_fields() -> usize {
                #n
            }
        }
    }
}
```

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
