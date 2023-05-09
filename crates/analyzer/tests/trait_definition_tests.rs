use ink_analyzer::Analysis;
use std::fs;

fn get_trait_definition_code(name: &str) -> String {
    fs::read_to_string(format!("tests/trait_definitions/{name}.rs")).unwrap()
}

#[test]
fn erc20_trait_works() {
    let diagnostics = Analysis.diagnostics(&get_trait_definition_code("erc20_trait"));

    dbg!(&diagnostics);
    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn flipper_trait_works() {
    let diagnostics = Analysis.diagnostics(&get_trait_definition_code("flipper_trait"));

    assert_eq!(diagnostics.len(), 0);
}
