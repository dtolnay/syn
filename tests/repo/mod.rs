#![allow(clippy::manual_assert)]

mod progress;

use self::progress::Progress;
use anyhow::Result;
use flate2::read::GzDecoder;
use std::fs;
use std::path::Path;
use tar::Archive;
use walkdir::DirEntry;

const REVISION: &str = "98ad6a5519651af36e246c0335c964dd52c554ba";

#[rustfmt::skip]
static EXCLUDE_FILES: &[&str] = &[
    // TODO
    "compiler/rustc_codegen_gcc/src/intrinsic/archs.rs",
    "src/test/run-make/translation/test.rs",
    "src/test/rustdoc-ui/issue-79467.rs",
    "src/test/ui/dyn-star/const.rs",
    "src/test/ui/dyn-star/drop.rs",
    "src/test/ui/dyn-star/make-dyn-star.rs",
    "src/test/ui/dyn-star/method.rs",
    "src/test/ui/dyn-star/syntax.rs",
    "src/test/ui/generics/issue-94432-garbage-ice.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/empty_exponent.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/empty_int.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/lifetime_starts_with_a_number.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_block_comment_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_block_comment_with_content.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_slash_double_quote.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_string_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_slash_single_quote.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_byte_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_slash_single_quote.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_char_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_nested_block_comment_entirely.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_nested_block_comment_partially.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_byte_string_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_raw_string_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_ascii_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_ferris.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_slash.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_slash_double_quote.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_slash_n.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_space.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unclosed_string_with_unicode_escape.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unstarted_raw_byte_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unstarted_raw_byte_string_with_ascii.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unstarted_raw_string_at_eof.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/err/unstarted_raw_string_with_ascii.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/byte_strings.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/chars.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/hello.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/ident.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/keywords.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/lifetimes.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/numbers.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/raw_ident.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/raw_strings.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/single_line_comments.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/strings.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/symbols.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/lexer/ok/whitespace.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0000_struct_field_missing_comma.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0001_item_recovery_in_file.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0002_duplicate_shebang.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0003_C++_semicolon.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0004_use_path_bad_segment.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0005_attribute_recover.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0006_named_field_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0007_stray_curly_in_file.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0008_item_block_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0009_broken_struct_type_parameter.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0010_unsafe_lambda_block.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0011_extern_struct.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0013_invalid_type.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0014_where_no_bounds.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0015_curly_in_params.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0016_missing_semi.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0017_incomplete_binexpr.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0018_incomplete_fn.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0019_let_recover.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0020_fn_recover.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0021_incomplete_param.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0022_bad_exprs.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0023_mismatched_paren.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0025_nope.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0026_imp_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0027_incomplere_where_for.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0029_field_completion.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0032_match_arms_inner_attrs.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0033_match_arms_outer_attrs.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0034_bad_box_pattern.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0035_use_recover.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0036_partial_use.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0039_lambda_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0042_weird_blocks.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0043_unexpected_for_type.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0044_item_modifiers.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0047_repated_extern_modifier.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/err/0048_double_fish.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0001_array_type_missing_semi.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0002_misplaced_label_err.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0003_pointer_type_no_mutability.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0005_fn_pointer_type_missing_fn.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0006_unsafe_block_in_mod.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0007_async_without_semicolon.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0008_pub_expr.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0013_anonymous_static.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0014_record_literal_before_ellipsis_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0014_record_literal_missing_ellipsis_recovery.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0014_struct_field_recover.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0015_empty_segment.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/err/0015_missing_fn_param_type.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0012_type_item_where_clause.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0040_crate_keyword_vis.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0102_record_pat_field_list.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0106_lambda_expr.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0131_existential_type.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0138_associated_type_bounds.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0179_use_tree_abs_star.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0188_const_param_default_path.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0200_const_param_default_literal.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/inline/ok/0202_typepathfn_with_coloncolon.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0015_use_tree.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0029_range_forms.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0051_parameter_attrs.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0055_dot_dot_dot.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0063_variadic_fun.rs",
    "src/tools/rust-analyzer/crates/parser/test_data/parser/ok/0068_item_modifiers.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/fuzz-failures/0000.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/fuzz-failures/0001.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/fuzz-failures/0002.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/fuzz-failures/0003.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/fuzz-failures/0004.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/validation/0031_block_inner_attrs.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/validation/0045_ambiguous_trait_object.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/parser/validation/0046_mutable_const_item.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0000.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0001.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0002.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0003.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0004.rs",
    "src/tools/rust-analyzer/crates/syntax/test_data/reparse/fuzz-failures/0005.rs",
    "src/tools/rustfmt/tests/source/closure.rs",
    "src/tools/rustfmt/tests/target/closure.rs",

    // TODO: impl ~const T {}
    // https://github.com/dtolnay/syn/issues/1051
    "src/test/ui/rfc-2632-const-trait-impl/syntax.rs",

    // Compile-fail expr parameter in const generic position: f::<1 + 2>()
    "src/test/ui/const-generics/early/closing-args-token.rs",
    "src/test/ui/const-generics/early/const-expression-parameter.rs",

    // Need at least one trait in impl Trait, no such type as impl 'static
    "src/test/ui/type-alias-impl-trait/generic_type_does_not_live_long_enough.rs",

    // Deprecated anonymous parameter syntax in traits
    "src/test/ui/issues/issue-13105.rs",
    "src/test/ui/issues/issue-13775.rs",
    "src/test/ui/issues/issue-34074.rs",
    "src/test/ui/proc-macro/trait-fn-args-2015.rs",
    "src/tools/rustfmt/tests/source/trait.rs",
    "src/tools/rustfmt/tests/target/trait.rs",

    // Placeholder syntax for "throw expressions"
    "src/test/pretty/yeet-expr.rs",
    "src/test/ui/try-trait/yeet-for-option.rs",
    "src/test/ui/try-trait/yeet-for-result.rs",

    // Excessive nesting
    "src/test/ui/issues/issue-74564-if-expr-stack-overflow.rs",

    // Testing rustfmt on invalid syntax
    "src/tools/rustfmt/tests/coverage/target/comments.rs",
    "src/tools/rustfmt/tests/parser/issue-4126/invalid.rs",
    "src/tools/rustfmt/tests/parser/issue_4418.rs",
    "src/tools/rustfmt/tests/parser/unclosed-delims/issue_4466.rs",
    "src/tools/rustfmt/tests/source/configs/disable_all_formatting/true.rs",
    "src/tools/rustfmt/tests/source/configs/spaces_around_ranges/false.rs",
    "src/tools/rustfmt/tests/source/configs/spaces_around_ranges/true.rs",
    "src/tools/rustfmt/tests/source/type.rs",
    "src/tools/rustfmt/tests/target/configs/spaces_around_ranges/false.rs",
    "src/tools/rustfmt/tests/target/configs/spaces_around_ranges/true.rs",
    "src/tools/rustfmt/tests/target/type.rs",

    // Clippy lint lists represented as expressions
    "src/tools/clippy/clippy_lints/src/lib.deprecated.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_all.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_cargo.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_complexity.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_correctness.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_internal.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_lints.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_nursery.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_pedantic.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_perf.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_restriction.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_style.rs",
    "src/tools/clippy/clippy_lints/src/lib.register_suspicious.rs",

    // Not actually test cases
    "src/test/rustdoc-ui/test-compile-fail2.rs",
    "src/test/rustdoc-ui/test-compile-fail3.rs",
    "src/test/ui/lint/expansion-time-include.rs",
    "src/test/ui/macros/auxiliary/macro-comma-support.rs",
    "src/test/ui/macros/auxiliary/macro-include-items-expr.rs",
    "src/test/ui/macros/include-single-expr-helper.rs",
    "src/test/ui/macros/include-single-expr-helper-1.rs",
    "src/test/ui/parser/issues/auxiliary/issue-21146-inc.rs",
];

#[rustfmt::skip]
static EXCLUDE_DIRS: &[&str] = &[];

pub fn base_dir_filter(entry: &DirEntry) -> bool {
    let path = entry.path();

    let mut path_string = path.to_string_lossy();
    if cfg!(windows) {
        path_string = path_string.replace('\\', "/").into();
    }
    let path_string = if path_string == "tests/rust" {
        return true;
    } else if let Some(path) = path_string.strip_prefix("tests/rust/") {
        path
    } else {
        panic!("unexpected path in Rust dist: {}", path_string);
    };

    if path.is_dir() {
        return !EXCLUDE_DIRS.contains(&path_string);
    }

    if path.extension().map_or(true, |e| e != "rs") {
        return false;
    }

    if path_string.starts_with("src/test/ui") {
        let stderr_path = path.with_extension("stderr");
        if stderr_path.exists() {
            // Expected to fail in some way
            return false;
        }
    }

    !EXCLUDE_FILES.contains(&path_string)
}

#[allow(dead_code)]
pub fn edition(path: &Path) -> &'static str {
    if path.ends_with("dyn-2015-no-warnings-without-lints.rs") {
        "2015"
    } else {
        "2018"
    }
}

pub fn clone_rust() {
    let needs_clone = match fs::read_to_string("tests/rust/COMMIT") {
        Err(_) => true,
        Ok(contents) => contents.trim() != REVISION,
    };
    if needs_clone {
        download_and_unpack().unwrap();
    }
    let mut missing = String::new();
    let test_src = Path::new("tests/rust");
    for exclude in EXCLUDE_FILES {
        if !test_src.join(exclude).is_file() {
            missing += "\ntests/rust/";
            missing += exclude;
        }
    }
    for exclude in EXCLUDE_DIRS {
        if !test_src.join(exclude).is_dir() {
            missing += "\ntests/rust/";
            missing += exclude;
            missing += "/";
        }
    }
    if !missing.is_empty() {
        panic!("excluded test file does not exist:{}\n", missing);
    }
}

fn download_and_unpack() -> Result<()> {
    let url = format!(
        "https://github.com/rust-lang/rust/archive/{}.tar.gz",
        REVISION
    );
    let response = reqwest::blocking::get(&url)?.error_for_status()?;
    let progress = Progress::new(response);
    let decoder = GzDecoder::new(progress);
    let mut archive = Archive::new(decoder);
    let prefix = format!("rust-{}", REVISION);

    let tests_rust = Path::new("tests/rust");
    if tests_rust.exists() {
        fs::remove_dir_all(tests_rust)?;
    }

    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?;
        if path == Path::new("pax_global_header") {
            continue;
        }
        let relative = path.strip_prefix(&prefix)?;
        let out = tests_rust.join(relative);
        entry.unpack(&out)?;
    }

    fs::write("tests/rust/COMMIT", REVISION)?;
    Ok(())
}
