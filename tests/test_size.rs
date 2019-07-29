#![cfg(target_pointer_width = "64")]

mod features;

use std::mem;
use syn::*;

#[test]
fn test_expr_size() {
    assert_eq!(mem::size_of::<Expr>(), 176);
}

#[test]
fn test_item_size() {
    assert_eq!(mem::size_of::<Item>(), 280);
}

#[test]
fn test_type_size() {
    assert_eq!(mem::size_of::<Type>(), 200);
}

#[test]
fn test_pat_size() {
    assert_eq!(mem::size_of::<Pat>(), 144);
}
