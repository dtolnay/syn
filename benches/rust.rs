// $ cargo bench --features full --bench rust

#![recursion_limit = "256"]
#![feature(rustc_private)]

extern crate rustc_data_structures;
extern crate syntax;
extern crate syntax_pos;

#[macro_use]
#[path = "../tests/macros/mod.rs"]
mod macros;

#[path = "../tests/common/mod.rs"]
mod common;

use proc_macro2::TokenStream;
use rustc_data_structures::sync::Lrc;
use std::fs;
use std::str::FromStr;
use std::time::{Duration, Instant};
use syntax::edition::Edition;
use syntax::errors::{emitter::Emitter, DiagnosticBuilder, Handler};
use syntax::parse::ParseSess;
use syntax::source_map::{FilePathMapping, SourceMap};
use syntax_pos::FileName;

fn tokenstream_parse(content: &str) -> Result<(), ()> {
    TokenStream::from_str(content).map(drop).map_err(drop)
}

fn syn_parse(content: &str) -> Result<(), ()> {
    syn::parse_file(content).map(drop).map_err(drop)
}

fn libsyntax_parse(content: &str) -> Result<(), ()> {
    struct SilentEmitter;

    impl Emitter for SilentEmitter {
        fn emit_diagnostic(&mut self, _db: &DiagnosticBuilder) {}
    }

    syntax::with_globals(Edition::Edition2018, || {
        let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
        let emitter = Box::new(SilentEmitter);
        let handler = Handler::with_emitter(false, None, emitter);
        let sess = ParseSess::with_span_handler(handler, cm);
        if let Err(mut diagnostic) = syntax::parse::parse_crate_from_source_str(
            FileName::Custom("bench".to_owned()),
            content.to_owned(),
            &sess,
        ) {
            diagnostic.cancel();
            return Err(());
        };
        Ok(())
    })
}

fn read_from_disk(content: &str) -> Result<(), ()> {
    let _ = content;
    Ok(())
}

fn exec(mut codepath: impl FnMut(&str) -> Result<(), ()>) -> Duration {
    let begin = Instant::now();
    let mut success = 0;
    let mut total = 0;

    walkdir::WalkDir::new("tests/rust/src")
        .into_iter()
        .filter_entry(common::base_dir_filter)
        .for_each(|entry| {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                return;
            }
            let content = fs::read_to_string(path).unwrap();
            let ok = codepath(&content).is_ok();
            success += ok as usize;
            total += 1;
            if !ok {
                eprintln!("FAIL {}", path.display());
            }
        });

    assert_eq!(success, total);
    begin.elapsed()
}

fn main() {
    common::clone_rust();

    macro_rules! testcases {
        ($($name:ident,)*) => {
            vec![
                $(
                    (stringify!($name), $name as fn(&str) -> Result<(), ()>),
                )*
            ]
        };
    }

    let mut lines = 0;
    let mut files = 0;
    exec(|content| {
        lines += content.lines().count();
        files += 1;
        Ok(())
    });
    eprintln!("\n{} lines in {} files", lines, files);

    for (name, f) in testcases!(
        read_from_disk,
        tokenstream_parse,
        syn_parse,
        libsyntax_parse,
    ) {
        eprint!("{:20}", format!("{}:", name));
        let elapsed = exec(f);
        eprintln!(
            "elapsed={}.{:03}s",
            elapsed.as_secs(),
            elapsed.subsec_millis(),
        );
    }
    eprintln!();
}
