use std::env;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-cfg=syn_non_exhaustive");
    println!("cargo:rustc-check-cfg=cfg(syn_non_exhaustive)");
    println!("cargo:rustc-check-cfg=cfg(feature, values(\"derive\", \"full\"))");

    let features = match env::var_os("DEP_SYN2_FEATURES") {
        Some(features) => features,
        None => "derive,full".into(),
    };
    if !features.is_empty() {
        for feature in features.to_str().unwrap().split(",") {
            println!("cargo:rustc-cfg=feature={:?}", feature);
        }
    }
}
