use crate::types;

use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::Path;

pub fn generate(defs: &types::Definitions) {
    let codegen_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let mut f = File::open(codegen_root.join("../Cargo.toml")).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let manifest: Manifest = toml::from_str(&s).unwrap();

    let introspect = Introspect {
        version: manifest.package.version,
        types: defs.types.clone(),
        tokens: defs.tokens.clone(),
    };

    let j = serde_json::to_string_pretty(&introspect).unwrap();
    let check: Introspect = serde_json::from_str(&j).unwrap();
    assert_eq!(introspect, check);

    let json_path = codegen_root.join("../syn.json");
    fs::write(json_path, j).unwrap();
}

#[derive(Debug, Deserialize)]
struct Manifest {
    package: Package,
}

#[derive(Debug, Deserialize)]
struct Package {
    version: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Introspect {
    /// The `syn` version used to generate the introspection file
    version: String,
    types: Vec<types::Node>,
    tokens: BTreeMap<String, String>,
}
