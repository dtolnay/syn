#![cfg(feature = "full")]

#[macro_use]
extern crate quote;
extern crate syn;
extern crate syntex_syntax;
extern crate walkdir;

use syntex_syntax::ast;
use syntex_syntax::parse::{self, ParseSess, PResult};
use syntex_syntax::codemap::FilePathMapping;
use walkdir::{WalkDir, WalkDirIterator};

use std::fs::File;
use std::io::Read;
use std::panic;
use std::time::Instant;

#[allow(dead_code)]
#[macro_use]
mod common;

#[test]
fn test_round_trip() {
    common::check_min_stack();
    common::clone_rust();
    let abort_after = common::abort_after();
    if abort_after == 0 {
        panic!("Skipping all round_trip tests");
    }

    let mut failed = 0;

    let walk = WalkDir::new("tests/rust").sort_by(|a, b| a.cmp(b));
    for entry in walk.into_iter().filter_entry(common::base_dir_filter) {
        let entry = entry.unwrap();

        let path = entry.path();
        if path.is_dir() {
            continue;
        }
        errorf!("=== {}: ", path.display());

        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();

        let start = Instant::now();
        let (krate, elapsed) = match syn::parse_file(&content) {
            Ok(krate) => (krate, start.elapsed()),
            Err(msg) => {
                errorf!("syn failed to parse\n{:?}\n", msg);
                failed += 1;
                if failed >= abort_after {
                    panic!("Aborting Immediately due to ABORT_AFTER_FAILURE");
                }
                continue;
            }
        };
        let back = quote!(#krate).to_string();

        let equal = panic::catch_unwind(|| {
            let sess = ParseSess::new(FilePathMapping::empty());
            let before = match syntex_parse(content, &sess) {
                Ok(before) => before,
                Err(mut diagnostic) => {
                    diagnostic.cancel();
                    if diagnostic.message().starts_with("file not found for module") {
                        errorf!("ignore\n");
                    } else {
                        errorf!("ignore - syntex failed to parse original content: {}\n", diagnostic.message());
                    }
                    return true;
                }
            };
            let after = match syntex_parse(back, &sess) {
                Ok(after) => after,
                Err(mut diagnostic) => {
                    errorf!("syntex failed to parse");
                    diagnostic.emit();
                    return false;
                }
            };

            if before == after {
                errorf!("pass in {}ms\n",
                        elapsed.as_secs() * 1000 + elapsed.subsec_nanos() as u64 / 1_000_000);
                true
            } else {
                errorf!("FAIL\nbefore: {}\nafter: {}\n",
                        format!("{:?}", before).replace("\n", ""),
                        format!("{:?}", after).replace("\n", ""));
                false
            }
        });
        match equal {
            Err(_) => errorf!("ignoring syntex panic\n"),
            Ok(true) => {}
            Ok(false) => {
                failed += 1;
                if failed >= abort_after {
                    panic!("Aborting Immediately due to ABORT_AFTER_FAILURE");
                }
            },
        }
    }

    if failed > 0 {
        panic!("{} failures", failed);
    }
}

fn syntex_parse(content: String, sess: &ParseSess) -> PResult<ast::Crate> {
    let name = "test_round_trip".to_string();
    parse::parse_crate_from_source_str(name, content, sess)
        .map(common::respan::respan_crate)
}
