use crate::types::Definitions;

use std::fs;
use std::path::Path;

pub fn generate(defs: &Definitions) {
    let mut j = serde_json::to_string_pretty(&defs).unwrap();
    j.push('\n');

    let check: Definitions = serde_json::from_str(&j).unwrap();
    assert_eq!(*defs, check);

    let codegen_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let json_path = codegen_root.join("../syn.json");
    fs::write(json_path, j).unwrap();
}
