extern crate syn_error_experiment;

use syn_error_experiment::*;

fn main() {
    let input = "struct S { a: A, b: B, }";
    println!("{:#?}", parse_str::<Item>(input).unwrap());

    let input = "enum E { A, B, }";
    println!("{:#?}", parse_str::<Item>(input).unwrap());
}
