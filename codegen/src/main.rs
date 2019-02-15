//! This crate automatically generates the definition of the `Visit`,
//! `VisitMut`, and `Fold` traits in `syn` based on the `syn` source. It
//! discovers structs and enums declared with the `ast_*` macros and generates
//! the functions for those types.
//!
//! It makes a few assumptions about the target crate:
//! 1. All structs which are discovered must be re-exported in the root of the
//!    crate, even if they were declared in a submodule.
//! 2. This code cannot discover submodules which are located in subdirectories
//!    - only submodules located in the same directory.
//! 3. The path to `syn` is hardcoded.

#![recursion_limit = "128"]
#![allow(clippy::needless_pass_by_value)]

extern crate indexmap;
extern crate inflections;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate rustfmt_nightly as rustfmt;
#[macro_use]
extern crate syn;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate toml;

mod gen;
mod json;
mod parse;
mod types;

fn main() {
    let types = parse::parse();
    gen::generate(&types);
    json::generate(&types);
}
