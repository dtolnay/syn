extern crate syn;

mod features;

#[macro_use]
mod macros;

use syn::{Expr, Item};

#[test]
fn test_async_fn() {
    let code = "async fn process() {}";
    snapshot!(code as Item);
}

#[test]
fn test_async_closure() {
    let code = "async || {}";
    snapshot!(code as Expr);
}
