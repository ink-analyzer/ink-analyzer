//! Utilities for ink! analyzer integration tests.

use std::fs;

pub fn get_source_code(location: &str) -> String {
    fs::read_to_string(format!("tests/test_data/{location}.rs")).unwrap()
}
