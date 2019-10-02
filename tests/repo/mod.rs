use std::fs::{create_dir_all, read_to_string, remove_dir_all, rename, File};
use std::io::{self, prelude::*};
use std::path::Path;
use walkdir::DirEntry;

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

pub fn clone_rust() -> io::Result<()> {
    if match read_to_string("tests/rust/COMMIT") {
        Err(_) => true,
        Ok(contents) => contents.trim() != REVISION,
    } {
        let url = format!("https://github.com/rust-lang/rust/archive/{}.zip", REVISION);
        let mut tmpfile = tempfile::tempfile().unwrap();
        if let Ok(_request) = reqwest::get(url.as_str()).unwrap().copy_to(&mut tmpfile){
            let mut zip = zip::ZipArchive::new(tmpfile).unwrap();
            for i in 0..zip.len() {
                let mut file = zip.by_index(i).unwrap();
                let outpath = file.sanitized_name();
                if file.name().ends_with('/') {
                    create_dir_all(&outpath).unwrap()
                }
                else {
                    let mut outfile = File::create(&outpath).unwrap();
                    io::copy(&mut file, &mut outfile).unwrap();
                }
            }
            let root_folder = zip.by_index(0).unwrap().sanitized_name();
            let rust_path = Path::new("tests/rust");
            if rust_path.exists() {
                remove_dir_all(rust_path)?;
            }
            rename(root_folder, rust_path)?;
            if let Ok(mut commit) = File::create(&Path::new("tests/rust/COMMIT")) {
                commit.write_all(REVISION.as_bytes())?;
            }
        }
    }
    Ok(())
}
