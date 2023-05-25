use ink_analyzer::Analysis;
use std::fs;

fn get_contract_code(name: &str) -> String {
    fs::read_to_string(format!("tests/contracts/{name}.rs")).unwrap()
}

#[test]
fn erc20_works() {
    let diagnostics = Analysis::new(&get_contract_code("erc20")).diagnostics();

    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn flipper_works() {
    let diagnostics = Analysis::new(&get_contract_code("flipper")).diagnostics();

    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn mother_works() {
    let diagnostics = Analysis::new(&get_contract_code("mother")).diagnostics();

    assert_eq!(diagnostics.len(), 0);
}
