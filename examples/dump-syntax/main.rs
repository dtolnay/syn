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

use proc_macro2::Span;

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

    let syntax = match syn::parse_file(&src) {
        Ok(syntax) => syntax,
        Err(parse_error) => {
            let span = parse_error.span();
            show_location(span, src);
            return Err(Error::ParseFile(parse_error));
        }
    };

    println!("{:#?}", syntax);

    Ok(())
}

fn show_location(span: Span, src: String) {
    let start = span.start();
    let mut end = span.end();

    if start.line == end.line && start.column == end.column {
        return;
    }

    let src_line = match src.lines().nth(start.line - 1) {
        Some(line) => line,
        None => return,
    };

    if end.line > start.line {
        end.line = start.line;
        end.column = src_line.len();
    }

    let underline = " ".repeat(start.column) + &"^".repeat(end.column - start.column);

    let stderr = io::stderr();
    let mut stderr = stderr.lock();
    let _ = writeln!(stderr, "\n{}\n{}\n", src_line, underline);
}
