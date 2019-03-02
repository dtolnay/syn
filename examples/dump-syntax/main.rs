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
    ParseFile(syn::Error, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match self {
            IncorrectUsage => write!(f, "Usage: dump-syntax path/to/filename.rs"),
            ReadFile(err) => write!(f, "Unable to read file: {}", err),
            ParseFile(err, code) => render_location(f, err, code),
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
    let syntax = syn::parse_file(&src).map_err(|parse_error| Error::ParseFile(parse_error, src))?;
    println!("{:#?}", syntax);

    Ok(())
}

fn render_location(formatter: &mut fmt::Formatter, err: &syn::Error, code: &str) -> fmt::Result {
    let start = err.span().start();
    let mut end = err.span().end();

    if start.line == end.line && start.column == end.column {
        return Ok(());
    }

    let code_line = match code.lines().nth(start.line - 1) {
        Some(line) => line,
        None => return Ok(()),
    };

    if end.line > start.line {
        end.line = start.line;
        end.column = code_line.len();
    }

    write!(
        formatter,
        "\n\
         error: Syn unable to parse file\n\
         {indent}--> {filename}:{linenum}:{colnum}\n\
         {indent} | \n\
         {linenum} | {code}\n\
         {indent} | {offset}{underline} {message}\n\
         ",
        filename = "main.rs",
        linenum = start.line,
        colnum = start.column,
        indent = " ".repeat(start.line.to_string().len()),
        code = code_line,
        offset = " ".repeat(start.column),
        underline = "^".repeat(end.column - start.column),
        message = err,
    )
}
