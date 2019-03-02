//! Parse a Rust source file into a `syn::File` and print out a debug
//! representation of the syntax tree.
//!
//! Use the following command from this directory to test this program by
//! running it on its own source code:
//!
//!     cargo run -- main.rs
//!
//! The output will begin with:
//!
//!     File {
//!         shebang: None,
//!         attrs: [
//!             Attribute {
//!                 pound_token: Pound,
//!                 style: Inner(
//!         ...
//!     }

use std::env;
use std::fmt::{self, Display};
use std::fs;
use std::io::{self, Write};
use std::process;

enum Error {
    IncorrectUsage,
    ReadFile(io::Error),
    ParseFile(syn::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match self {
            IncorrectUsage => write!(f, "Usage: dump-syntax path/to/filename.rs"),
            ReadFile(e) => write!(f, "Unable to read file: {}", e),
            ParseFile(e) => write!(f, "Unable to parse file: {}", e),
        }
    }
}

fn main() {
    if let Err(error) = try_main() {
        let _ = writeln!(io::stderr(), "{}", error);
        process::exit(1);
    }
}

fn try_main() -> Result<(), Error> {
    let mut args = env::args();
    let _ = args.next(); // executable name

    let filename = match (args.next(), args.next()) {
        (Some(filename), None) => filename,
        _ => return Err(Error::IncorrectUsage),
    };

    let src = fs::read_to_string(filename).map_err(Error::ReadFile)?;
    let syntax = syn::parse_file(&src).map_err(Error::ParseFile)?;
    println!("{:#?}", syntax);

    Ok(())
}
