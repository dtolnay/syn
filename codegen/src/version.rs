use crate::workspace_path;
use anyhow::Result;
use semver::Version;
use serde::de::{Deserialize, Deserializer, IgnoredAny, MapAccess, Visitor};
use std::fmt;
use std::fs;

pub fn get() -> Result<Version> {
    let syn_cargo_toml = workspace_path::get("Cargo.toml");
    let manifest = fs::read_to_string(syn_cargo_toml)?;
    let parsed: Manifest = toml::from_str(&manifest)?;
    Ok(parsed.package.version)
}

#[derive(Debug)]
struct Manifest {
    package: Package,
}

#[derive(Debug)]
struct Package {
    version: Version,
}

impl<'de> Deserialize<'de> for Manifest {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ManifestVisitor;

        impl<'de> Visitor<'de> for ManifestVisitor {
            type Value = Manifest;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Manifest")
            }

            fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
            where
                M: MapAccess<'de>,
            {
                let mut package = None;
                while let Some(k) = map.next_key::<String>()? {
                    if k == "package" {
                        if package.is_some() {
                            return Err(serde::de::Error::duplicate_field("package"));
                        }
                        package = Some(map.next_value()?);
                    } else {
                        map.next_value::<IgnoredAny>()?;
                    }
                }
                let package = package.ok_or_else(|| serde::de::Error::missing_field("package"))?;
                Ok(Manifest { package })
            }
        }

        deserializer.deserialize_struct("Manifest", &["package"], ManifestVisitor)
    }
}

impl<'de> Deserialize<'de> for Package {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct PackageVisitor;

        impl<'de> Visitor<'de> for PackageVisitor {
            type Value = Package;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Package")
            }

            fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
            where
                M: MapAccess<'de>,
            {
                let mut version = None;
                while let Some(k) = map.next_key::<String>()? {
                    if k == "version" {
                        if version.is_some() {
                            return Err(serde::de::Error::duplicate_field("version"));
                        }
                        version = Some(map.next_value()?);
                    } else {
                        map.next_value::<IgnoredAny>()?;
                    }
                }
                let version = version.ok_or_else(|| serde::de::Error::missing_field("version"))?;
                Ok(Package { version })
            }
        }

        deserializer.deserialize_struct("Package", &["version"], PackageVisitor)
    }
}
