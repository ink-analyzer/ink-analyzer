use ink_analyzer::Analysis;
use std::fs;

fn get_chain_extension_code(name: &str) -> String {
    fs::read_to_string(format!("tests/chain_extensions/{name}.rs")).unwrap()
}

#[test]
fn psp22_extension_works() {
    let diagnostics = Analysis::new(&get_chain_extension_code("psp22_extension")).diagnostics();

    assert_eq!(diagnostics.len(), 0);
}

#[test]
fn rand_extension_works() {
    let diagnostics = Analysis::new(&get_chain_extension_code("rand_extension")).diagnostics();

    assert_eq!(diagnostics.len(), 0);
}
