//! integration tests for ink! analyzer diagnostics.

use ink_analyzer::Analysis;

mod utils;

#[test]
fn diagnostics_works() {
    for location in [
        // Contracts.
        "contracts/erc20",
        "contracts/flipper",
        "contracts/mother",
        // Chain extensions.
        "chain_extensions/psp22_extension",
        "chain_extensions/rand_extension",
        // Storage items.
        "storage_items/default_storage_key_1",
        "storage_items/non_packed_tuple_struct",
        "storage_items/complex_non_packed_struct",
        "storage_items/complex_non_packed_enum",
        "storage_items/complex_packed_struct",
        "storage_items/complex_packed_enum",
        // Trait definitions.
        "trait_definitions/erc20_trait",
        "trait_definitions/flipper_trait",
    ] {
        // Get source code.
        let code = utils::get_source_code(location);

        // Run diagnostics.
        let results = Analysis::new(&code).diagnostics();

        // Verify results.
        assert!(results.is_empty());
    }
}
