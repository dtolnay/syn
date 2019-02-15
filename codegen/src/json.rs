use crate::types;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn generate(types: &[types::TypeDef]) {
    let codegen_root = Path::new(env!("CARGO_MANIFEST_DIR"));

    let mut f = File::open(codegen_root.join("../Cargo.toml")).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let manifest: Manifest = toml::from_str(&s).unwrap();

    let f = ::std::fs::File::create(codegen_root.join("../syn.json")).unwrap();

    serde_json::to_writer_pretty(f, &Introspect {
        version: &manifest.package.version,
        types,
    }).unwrap();
}

#[derive(Debug, Deserialize)]
struct Manifest {
    package: Package,
}

#[derive(Debug, Deserialize)]
struct Package {
    version: String,
}

#[derive(Debug, Serialize)]
struct Introspect<'a> {
    /// The `syn` version used to generate the introspection file
    version: &'a str,
    types: &'a [types::TypeDef],
}
