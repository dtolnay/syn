use flate2::read::GzDecoder;
use std::fs::{read_to_string, File};
use std::io::{Seek, SeekFrom};
use std::path::Path;
use tar::Archive;

static REVISION: &'static str = "521d78407471cb78e9bbf47160f6aa23047ac499";

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
        // Deprecated placement syntax
        "tests/rust/src/test/ui/obsolete-in-place/bad.rs" |
        // Deprecated anonymous parameter syntax in traits
        "tests/rust/src/test/ui/error-codes/e0119/auxiliary/issue-23563-a.rs" |
        "tests/rust/src/test/ui/issues/issue-13105.rs" |
        "tests/rust/src/test/ui/issues/issue-13775.rs" |
        "tests/rust/src/test/ui/issues/issue-34074.rs" |
        // Deprecated await macro syntax
        "tests/rust/src/test/ui/async-await/await-macro.rs" |
        // 2015-style dyn that libsyntax rejects
        "tests/rust/src/test/ui/dyn-keyword/dyn-2015-no-warnings-without-lints.rs" |
        // not actually test cases
        "tests/rust/src/test/ui/macros/auxiliary/macro-comma-support.rs" |
        "tests/rust/src/test/ui/macros/auxiliary/macro-include-items-expr.rs" |
        "tests/rust/src/test/ui/issues/auxiliary/issue-21146-inc.rs" => false,
        _ => true,
    }
}

pub fn clone_rust() -> Result<(), Box<dyn std::error::Error>> {
    if match read_to_string("tests/rust/COMMIT") {
        Err(_) => true,
        Ok(contents) => contents.trim() != REVISION,
    } {
        let url = format!(
            "https://github.com/rust-lang/rust/archive/{}.tar.gz",
            REVISION
        );
        let mut tmpfile = tempfile::tempfile().unwrap();
        let bytecount = reqwest::get(url.as_str()).unwrap().copy_to(&mut tmpfile)?;
        tmpfile.seek(SeekFrom::Start(0))?;
        let decoder = GzDecoder::new(tmpfile);
        let mut archive = Archive::new(decoder);
        let prefix = format!("rust-{}", REVISION);
        for mut entry in archive.entries()?.filter_map(|e| e.ok()) {
            if entry.path()?.starts_with(&prefix[..]) {
                let path =
                    Path::new("rust").join(entry.path()?.strip_prefix(&prefix[..])?.to_owned());
                entry.unpack(&path)?;
            }
        }
        fs::write("tests/rust/COMMIT", REVISION)?;
    }
    Ok(())
}
