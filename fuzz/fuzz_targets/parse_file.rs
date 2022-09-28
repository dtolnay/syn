#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

fuzz_target!(|data: &[u8]| {
    if data.len() < 300 {
        if let Ok(string) = str::from_utf8(data) {
            _ = syn::parse_file(string);
        }
    }
});
