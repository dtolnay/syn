#![feature(proc_macro)]

extern crate lazy_static;
extern crate regex;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref USERNAME: Regex = {
        println!("Compiling username regex...");
        Regex::new("^[a-z0-9_-]{3,16}$").unwrap()
    };
}

fn main() {
    println!("Let's validate some usernames.");
    validate("fergie");
    validate("will.i.am");
}

fn validate(name: &str) {
    println!("is_match({:?}): {}", name, USERNAME.is_match(name));
}
