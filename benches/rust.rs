// $ cargo bench --features full --bench rust
//
// Syn only, useful for profiling:
// $ RUSTFLAGS='--cfg syn_only' cargo build --release --features full --bench rust

#![cfg_attr(not(syn_only), feature(rustc_private))]

#[path = "../tests/repo/mod.rs"]
mod repo;

#[cfg(not(syn_only))]
mod imports {
    pub extern crate rustc_data_structures;
    pub extern crate syntax;
    pub extern crate syntax_pos;

    pub use proc_macro2::TokenStream;
    pub use rustc_data_structures::sync::Lrc;
    pub use std::str::FromStr;
    pub use syntax::edition::Edition;
    pub use syntax::errors::{emitter::Emitter, DiagnosticBuilder, Handler};
    pub use syntax::parse::ParseSess;
    pub use syntax::source_map::{FilePathMapping, SourceMap};
    pub use syntax_pos::FileName;
}

#[cfg(not(syn_only))]
use imports::*;

use std::fs;
use std::time::{Duration, Instant};

#[cfg(not(syn_only))]
fn tokenstream_parse(content: &str) -> Result<(), ()> {
    TokenStream::from_str(content).map(drop).map_err(drop)
}

fn syn_parse(content: &str) -> Result<(), ()> {
    syn::parse_file(content).map(drop).map_err(drop)
}

#[cfg(not(syn_only))]
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

#[cfg(not(syn_only))]
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
        .filter_entry(repo::base_dir_filter)
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
    repo::clone_rust();

    macro_rules! testcases {
        ($($(#[$cfg:meta])* $name:ident,)*) => {
            vec![
                $(
                    $(#[$cfg])*
                    (stringify!($name), $name as fn(&str) -> Result<(), ()>),
                )*
            ]
        };
    }

    #[cfg(not(syn_only))]
    {
        let mut lines = 0;
        let mut files = 0;
        exec(|content| {
            lines += content.lines().count();
            files += 1;
            Ok(())
        });
        eprintln!("\n{} lines in {} files", lines, files);
    }

    for (name, f) in testcases!(
        #[cfg(not(syn_only))]
        read_from_disk,
        #[cfg(not(syn_only))]
        tokenstream_parse,
        syn_parse,
        #[cfg(not(syn_only))]
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
