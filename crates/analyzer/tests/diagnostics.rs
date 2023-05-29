//! integration tests for ink! analyzer diagnostics.

use ink_analyzer::Analysis;
use test_utils::parse_offset_at;

mod utils;

#[test]
fn diagnostics_works() {
    for (source, test_cases) in [
        // (source, [Option<Vec<(rep_start_pat, rep_end_pat, replacement)>>, expected_results]) where:
        // source = location of the source code,
        // rep_start_pat = substring used to find the start offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // rep_end_pat = substring used to find the end offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // replacement = the replacement snippet that will inserted before tests are run on the modified source code,
        // n_diagnostics = the expected number of diagnostic errors/warnings.

        // Contracts.
        (
            "contracts/erc20",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "",
                    )]),
                    10, // 1 storage, 2 events, 1 constructor and 6 messages without a contract parent.
                ),
                (
                    Some(vec![(
                        Some("<-#[ink(storage)]"),
                        Some("#[ink(storage)]"),
                        "",
                    )]),
                    1, // missing storage.
                ),
                (
                    Some(vec![(
                        Some("<-#[ink(constructor)]"),
                        Some("#[ink(constructor)]"),
                        "",
                    )]),
                    1, // no constructor(s).
                ),
                (
                    Some(
                        (1..=6)
                            .map(|_| (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""))
                            .collect(),
                    ),
                    1, // no message(s).
                ),
            ],
        ),
        (
            "contracts/flipper",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "",
                    )]),
                    5, // 1 storage, 2 constructors and 2 messages without a contract parent.
                ),
                (
                    Some(vec![(
                        Some("<-#[ink(storage)]"),
                        Some("#[ink(storage)]"),
                        "",
                    )]),
                    1, // missing storage.
                ),
                (
                    Some(vec![
                        (
                            Some("<-#[ink(constructor)]"),
                            Some("#[ink(constructor)]"),
                            "",
                        ),
                        (
                            Some("<-#[ink(constructor)]"),
                            Some("#[ink(constructor)]"),
                            "",
                        ),
                    ]),
                    1, // no constructor(s).
                ),
                (
                    Some(vec![
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                    ]),
                    1, // no message(s).
                ),
            ],
        ),
        (
            "contracts/mother",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "",
                    )]),
                    8, // 1 storage, 1 event, 3 constructors and 3 messages without a contract parent.
                ),
                (
                    Some(vec![(
                        Some("<-#[ink(storage)]"),
                        Some("#[ink(storage)]"),
                        "",
                    )]),
                    1, // missing storage.
                ),
                (
                    Some(vec![
                        (
                            Some("<-#[ink(constructor)]"),
                            Some("#[ink(constructor)]"),
                            "",
                        ),
                        (
                            Some("<-#[ink(constructor)]"),
                            Some("#[ink(constructor)]"),
                            "",
                        ),
                        (
                            Some("<-#[ink(constructor)]"),
                            Some("#[ink(constructor)]"),
                            "",
                        ),
                    ]),
                    1, // no constructor(s).
                ),
                (
                    Some(vec![
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                    ]),
                    1, // no message(s).
                ),
            ],
        ),
        // Chain extensions.
        (
            "chain_extensions/psp22_extension",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                        "",
                    )]),
                    11, // 11 extensions without a chain extension parent.
                ),
                (
                    Some(vec![(
                        Some("<-type ErrorCode = Psp22Error;"),
                        Some("type ErrorCode = Psp22Error;"),
                        "",
                    )]),
                    1, // missing `ErrorCode` type.
                ),
            ],
        ),
        (
            "chain_extensions/rand_extension",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                        "",
                    )]),
                    1, // 1 extension without a chain extension parent.
                ),
                (
                    Some(vec![(
                        Some("<-type ErrorCode = RandomReadErr;"),
                        Some("type ErrorCode = RandomReadErr;"),
                        "",
                    )]),
                    1, // missing `ErrorCode` type.
                ),
            ],
        ),
        // Storage items.
        ("storage_items/default_storage_key_1", vec![(None, 0)]),
        ("storage_items/non_packed_tuple_struct", vec![(None, 0)]),
        ("storage_items/complex_non_packed_struct", vec![(None, 0)]),
        ("storage_items/complex_non_packed_enum", vec![(None, 0)]),
        ("storage_items/complex_packed_struct", vec![(None, 0)]),
        ("storage_items/complex_packed_enum", vec![(None, 0)]),
        // Trait definitions.
        (
            "trait_definitions/erc20_trait",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        "",
                    )]),
                    6, // 6 messages without a trait definition nor impl parent.
                ),
                (
                    Some(
                        (1..=6)
                            .map(|_| (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""))
                            .collect(),
                    ),
                    7, // 1 trait level "missing message(s)", 6 method level "not a message" errors.
                ),
            ],
        ),
        (
            "trait_definitions/flipper_trait",
            vec![
                (None, 0),
                (
                    Some(vec![(
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        "",
                    )]),
                    2, // 2 messages without a trait definition nor impl parent.
                ),
                (
                    Some(vec![
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                        (Some("<-#[ink(message)]"), Some("#[ink(message)]"), ""),
                    ]),
                    3, // 1 trait level "missing message(s)", 2 method level "not a message" errors.
                ),
            ],
        ),
    ] {
        // Gets the original source code.
        let original_code = utils::get_source_code(source);

        for (modifications, expected_results) in test_cases {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some(modifications) = modifications {
                for (rep_start_pat, rep_end_pat, replacement) in modifications {
                    let start_offset = parse_offset_at(&test_code, rep_start_pat).unwrap();
                    let end_offset = parse_offset_at(&test_code, rep_end_pat).unwrap();
                    test_code.replace_range(start_offset..end_offset, replacement);
                }
            }

            // Runs diagnostics.
            let results = Analysis::new(&test_code).diagnostics();

            // Verifies results.
            assert_eq!(results.len(), expected_results);
        }
    }
}
