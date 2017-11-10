extern crate syn;

use std::env;
use std::fs::File;
use std::io::{self, Read};

fn main() {
    let mut args = env::args();
    let _ = args.next(); // executable name
    let filename = args.next().unwrap_or_else(|| {
        panic!("USAGE: dump-ast FILENAME");
    });
    if args.next().is_some() {
        panic!("dump-ast only takes one argument");
    }

    let mut src = String::new();
    if filename != "-" {
        let mut file = File::open(&filename).expect("Unable to open source file");
        file.read_to_string(&mut src).expect("Unable to read input file");
    } else {
        io::stdin().read_to_string(&mut src).expect("Unable to read stdin");
    }

    let ast = syn::parse_file(&src).expect("Unable to parse file");
    println!("{:#?}", ast);
}
