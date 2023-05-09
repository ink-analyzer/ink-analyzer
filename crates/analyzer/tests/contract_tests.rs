use ink_analyzer::Analysis;
use std::fs;

fn get_contract_code(name: &str) -> String {
    fs::read_to_string(format!("tests/contracts/{name}.rs")).unwrap()
}

#[test]
fn erc20_works() {
    let diagnostics = Analysis.diagnostics(&get_contract_code("erc20"));

    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn flipper_works() {
    let diagnostics = Analysis.diagnostics(&get_contract_code("flipper"));

    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn mother_works() {
    let diagnostics = Analysis.diagnostics(&get_contract_code("mother"));

    assert_eq!(diagnostics.len(), 0);
}
