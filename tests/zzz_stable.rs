#![cfg(syn_disable_nightly_tests)]

extern crate colored;

use colored::Colorize;

const MSG: &str = "
‖
‖   WARNING:
‖   This is not a nightly compiler so not all tests were able to
‖   run. Syn includes tests that compare Syn's parser against the
‖   compiler's parser, which requires access to unstable libsyntax
‖   data structures and a nightly compiler.
‖
";

#[test]
fn notice() {
    panic!(MSG
        .replace("WARNING", &"WARNING".bold().to_string())
        .yellow()
        .to_string());
}
