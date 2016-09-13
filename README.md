Nom parser for Rust items
=========================

[![Build Status](https://api.travis-ci.org/dtolnay/syn.svg?branch=master)](https://travis-ci.org/dtolnay/syn)
[![Latest Version](https://img.shields.io/crates/v/syn.svg)](https://crates.io/crates/syn)
[![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://dtolnay.github.io/syn/syn/)

Parse Rust structs and enums without a Syntex dependency, intended for use with
[Macros 1.1](https://github.com/rust-lang/rfcs/blob/master/text/1681-macros-1.1.md).

Designed for fast compile time.

- Compile time for `syn` (from scratch including all dependencies): **6 seconds**
- Compile time for the `syntex`/`quasi`/`aster` stack: **60+ seconds**

```toml
[dependencies]
syn = "0.5"
```

```rust
extern crate syn;

let raw = "
    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Item {
        pub ident: Ident,
        pub vis: Visibility,
        pub attrs: Vec<Attribute>,
        pub generics: Generics,
        pub body: Body,
    }
";

let ast = syn::parse_item(raw).unwrap();
```

## Usage with [Macros 1.1](https://github.com/rust-lang/rfcs/blob/master/text/1681-macros-1.1.md)
```rust
// lib.rs
#![crate_type = "rustc-macro"]
#![feature(rustc_macro)]
#![feature(rustc_macro_lib)]

extern crate rustc_macro;
#[macro_use]
extern crate quote;
extern crate syn;

use rustc_macro::TokenStream;

#[rustc_macro_derive(SpecialItem)]
pub fn special_item(input: TokenStream) -> TokenStream {
    let source = input.to_string();

    // Parse a string of items to an AST
    let ast = syn::parse_item(&source).unwrap();

    // Parse this back to a token stream and return it
    quote!(#ast).to_string().parse().unwrap()
}
```
```toml
# Cargo.toml
# ...
[dependencies]
syn = "0.5"
quote = "0.1"

[lib]
rustc-macro = true
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
