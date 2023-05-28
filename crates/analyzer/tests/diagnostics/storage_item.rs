//! integration tests for ink! analyzer storage item diagnostics.

use ink_analyzer::Analysis;
use std::fs;

fn get_storage_item_code(name: &str) -> String {
    fs::read_to_string(format!("tests/test_data/storage_items/{name}.rs")).unwrap()
}

#[test]
fn default_storage_key_works() {
    let diagnostics = Analysis::new(&get_storage_item_code("default_storage_key_1")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn non_packed_tuple_struct_works() {
    let diagnostics =
        Analysis::new(&get_storage_item_code("non_packed_tuple_struct")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn complex_non_packed_struct_works() {
    let diagnostics =
        Analysis::new(&get_storage_item_code("complex_non_packed_struct")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn complex_non_packed_enum_works() {
    let diagnostics =
        Analysis::new(&get_storage_item_code("complex_non_packed_enum")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn complex_packed_struct_works() {
    let diagnostics = Analysis::new(&get_storage_item_code("complex_packed_struct")).diagnostics();

    assert!(diagnostics.is_empty());
}

#[test]
fn complex_packed_enum_works() {
    let diagnostics = Analysis::new(&get_storage_item_code("complex_packed_enum")).diagnostics();

    assert!(diagnostics.is_empty());
}
