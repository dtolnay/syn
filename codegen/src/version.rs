use crate::workspace_path;
use anyhow::Result;
use semver::Version;
use serde_derive::Deserialize;
use std::fs;

pub fn get() -> Result<Version> {
    let syn_cargo_toml = workspace_path::get("Cargo.toml");
    let manifest = fs::read_to_string(syn_cargo_toml)?;
    let parsed: Manifest = toml::from_str(&manifest)?;
    Ok(parsed.package.version)
}

#[derive(Debug, Deserialize)]
struct Manifest {
    package: Package,
}

#[derive(Debug, Deserialize)]
struct Package {
    version: Version,
}
