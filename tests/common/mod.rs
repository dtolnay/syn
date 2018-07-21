// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code)]

extern crate syntax;
extern crate walkdir;

use std;
use std::env;
use std::process::Command;

use self::walkdir::DirEntry;

pub mod eq;
pub mod parse;

pub fn check_min_stack() {
    let min_stack_value = match env::var("RUST_MIN_STACK") {
        Ok(s) => s,
        Err(_) => {
            env::set_var("RUST_MIN_STACK", 16000000.to_string());
            return;
        }
    };
    let min_stack_value: usize = min_stack_value
        .parse()
        .expect("RUST_MIN_STACK env var should be set since some tests require it.");
    assert!(min_stack_value >= 16_000_000);
}

/// Read the `ABORT_AFTER_FAILURE` environment variable, and parse it.
pub fn abort_after() -> usize {
    match env::var("ABORT_AFTER_FAILURE") {
        Ok(s) => s.parse().expect("failed to parse ABORT_AFTER_FAILURE"),
        Err(_) => std::usize::MAX,
    }
}

pub fn base_dir_filter(entry: &DirEntry) -> bool {
    let path = entry.path();
    if path.is_dir() {
        return true; // otherwise walkdir does not visit the files
    }
    if path.extension().map(|e| e != "rs").unwrap_or(true) {
        return false;
    }
    let path_string = path.to_string_lossy();
    let path_string = if cfg!(windows) {
        path_string.replace('\\', "/").into()
    } else {
        path_string
    };
    // TODO assert that parsing fails on the parse-fail cases
    if path_string.starts_with("tests/rust/src/test/parse-fail")
        || path_string.starts_with("tests/rust/src/test/compile-fail")
        || path_string.starts_with("tests/rust/src/test/rustfix")
    {
        return false;
    }

    if path_string.starts_with("tests/rust/src/test/ui") {
        let stderr_path = path.with_extension("stderr");
        if stderr_path.exists() {
            // Expected to fail in some way
            return false;
        }
    }

    match path_string.as_ref() {
        // TODO outer attributes on literals
        //
        //     #[noop] "Hello, world!"
        //     #[cfg(unset)] 341
        "tests/rust/src/test/run-pass-fulldeps/proc-macro/attr-stmt-expr.rs" |
        "tests/rust/src/test/run-pass/cfg_stmt_expr.rs" |
        // TODO outer attributes on closures
        //
        // https://github.com/dtolnay/syn/issues/457
        "tests/rust/src/test/run-pass/issue-49632.rs" |
        // TODO inner attributes within expressions
        //
        // https://github.com/dtolnay/syn/issues/458
        // https://github.com/dtolnay/syn/issues/459
        // https://github.com/dtolnay/syn/issues/460
        "tests/rust/src/test/pretty/stmt_expr_attributes.rs" |
        // TODO feature(macros_in_extern)
        //
        // https://github.com/dtolnay/syn/issues/390
        "tests/rust/src/test/run-pass-fulldeps/proc-macro/macros-in-extern.rs" |
        "tests/rust/src/test/run-pass/macros-in-extern.rs" |
        // TODO async closures and async functions
        //
        // https://github.com/dtolnay/syn/issues/396
        // https://github.com/dtolnay/syn/issues/395
        "tests/rust/src/test/run-pass/async-await.rs" |
        // TODO feature(label_break_value)
        //
        // https://github.com/dtolnay/syn/issues/455
        "tests/rust/src/test/run-pass/label_break_value.rs" |
        // Deprecated placement syntax
        "tests/rust/src/test/run-pass/new-box-syntax.rs" |
        "tests/rust/src/test/ui/obsolete-in-place/bad.rs" |
        // not actually test cases
        "tests/rust/src/test/run-pass/auxiliary/macro-comma-support.rs" |
        "tests/rust/src/test/run-pass/auxiliary/macro-include-items-expr.rs" => false,
        _ => true,
    }
}

pub fn clone_rust() {
    let result = Command::new("tests/clone.sh").status().unwrap();
    println!("result: {}", result);
    assert!(result.success());
}
