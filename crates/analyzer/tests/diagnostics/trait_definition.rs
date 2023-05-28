//! integration tests for ink! analyzer trait definition diagnostics.

use ink_analyzer::Analysis;
use std::fs;

fn get_trait_definition_code(name: &str) -> String {
    fs::read_to_string(format!("tests/test_data/trait_definitions/{name}.rs")).unwrap()
}

#[test]
fn erc20_trait_works() {
    let diagnostics = Analysis::new(&get_trait_definition_code("erc20_trait")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn flipper_trait_works() {
    let diagnostics = Analysis::new(&get_trait_definition_code("flipper_trait")).diagnostics();

    assert!(diagnostics.is_empty());
}
