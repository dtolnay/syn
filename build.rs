#![allow(clippy::let_underscore_untyped, clippy::manual_let_else)]

use std::env;
use std::process::Command;
use std::str;

// The rustc-cfg strings below are *not* public API. Please let us know by
// opening a GitHub issue if your build environment requires some way to enable
// these cfgs other than by executing our build script.
fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let compiler = match rustc_version() {
        Some(compiler) => compiler,
        None => return,
    };

    // Note: add "/build.rs" to package.include in Cargo.toml if adding any
    // conditional compilation within the library.
    let _ = compiler.minor;

    if !compiler.nightly {
        println!("cargo:rustc-cfg=syn_disable_nightly_tests");
    }
}

struct Compiler {
    minor: u32,
    nightly: bool,
}

fn rustc_version() -> Option<Compiler> {
    let rustc = env::var_os("RUSTC")?;
    let output = Command::new(rustc).arg("--version").output().ok()?;
    let version = str::from_utf8(&output.stdout).ok()?;
    let mut pieces = version.split('.');
    if pieces.next() != Some("rustc 1") {
        return None;
    }
    let minor = pieces.next()?.parse().ok()?;
    let nightly = version.contains("nightly") || version.ends_with("-dev");
    Some(Compiler { minor, nightly })
}
