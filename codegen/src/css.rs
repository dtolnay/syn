use crate::workspace_path;
use anyhow::Result;
use indoc::{formatdoc, indoc};
use std::fs;
use syn_codegen::Definitions;

pub fn generate(defs: &Definitions) -> Result<()> {
    let mut styles = String::new();
    for ty in defs.tokens.keys() {
        styles += &format!("a.struct[title=\"struct syn::token::{ty}\"],\n");
    }
    styles.truncate(styles.len() - 2);
    styles += indoc! {"
         {
        \tdisplay: inline-block;
        \tcolor: transparent;
        \twhite-space: nowrap;
        }
    "};
    styles.push('\n');
    for ty in defs.tokens.keys() {
        styles += &format!("a.struct[title=\"struct syn::token::{ty}\"]::before,\n");
    }
    styles.truncate(styles.len() - 2);
    styles += indoc! {"
         {
        \tdisplay: inline-block;
        \tcolor: var(--type-link-color);
        \twidth: 0;
        }
    "};
    for (ty, repr) in &defs.tokens {
        let padding = ".".repeat((repr.len() + 8).saturating_sub(ty.len()));
        styles += &formatdoc! {"

            a.struct[title=\"struct syn::token::{ty}\"]::before {{
            \tcontent: \"Token![{repr}]\";
            }}

            a.struct[title=\"struct syn::token::{ty}\"]::after {{
            \tcontent: \"{padding}\";
            }}
        "};
    }

    let css_path = workspace_path::get("src/gen/token.css");
    fs::write(css_path, styles)?;
    Ok(())
}
