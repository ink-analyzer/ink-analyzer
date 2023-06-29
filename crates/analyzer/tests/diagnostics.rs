//! integration tests for ink! analyzer diagnostics.

use ink_analyzer::Analysis;
use test_utils;
use test_utils::{TestCase, TestCaseModification, TestCaseResults, TestGroup};

// The high-level methodology for diagnostics test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (optionally) make modifications to the source code at specific offsets/text ranges to create a specific test case.
// - compute diagnostics for the modified source code.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn diagnostics_works() {
    // Iterates over all test case groups.
    for test_group in [
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test_data/contracts` directory.
            source: "contracts/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: vec![
                TestCase {
                    // Makes no modifications to the source code.
                    modifications: None,
                    // No parameters.
                    params: None,
                    // Expects no diagnostic errors/warnings.
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    // Removes `#[ink::contract]` from the source code.
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "",
                    }]),
                    // No parameters.
                    params: None,
                    // Expects 10 diagnostic errors/warnings (i.e 1 storage, 2 events, 1 constructor and 6 messages without a contract parent).
                    results: TestCaseResults::Diagnostic(10),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "",
                    }]),
                    params: None,
                    // missing storage.
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "",
                    }]),
                    params: None,
                    // no constructor(s).
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(
                        (1..=6)
                            .map(|_| TestCaseModification {
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("#[ink(message)]"),
                                replacement: "",
                            })
                            .collect(),
                    ),
                    params: None,
                    // no message(s).
                    results: TestCaseResults::Diagnostic(1),
                },
            ],
        },
        TestGroup {
            source: "contracts/flipper",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 1 storage, 2 constructors and 2 messages without a contract parent.
                    results: TestCaseResults::Diagnostic(5),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "",
                    }]),
                    params: None,
                    // missing storage.
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![
                        TestCaseModification {
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                            replacement: "",
                        },
                    ]),
                    params: None,
                    // no constructor(s).
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                    ]),
                    params: None,
                    // no message(s).
                    results: TestCaseResults::Diagnostic(1),
                },
            ],
        },
        TestGroup {
            source: "contracts/mother",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 1 storage, 1 event, 3 constructors and 3 messages without a contract parent.
                    results: TestCaseResults::Diagnostic(8),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "",
                    }]),
                    params: None,
                    // missing storage.
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![
                        TestCaseModification {
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                            replacement: "",
                        },
                    ]),
                    params: None,
                    // no constructor(s).
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                    ]),
                    params: None,
                    // no message(s).
                    results: TestCaseResults::Diagnostic(1),
                },
            ],
        },
        // Chain extensions.
        TestGroup {
            source: "chain_extensions/psp22_extension",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 11 extensions without a chain extension parent.
                    results: TestCaseResults::Diagnostic(11),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-type ErrorCode = Psp22Error;"),
                        end_pat: Some("type ErrorCode = Psp22Error;"),
                        replacement: "",
                    }]),
                    params: None,
                    // missing `ErrorCode` type.
                    results: TestCaseResults::Diagnostic(1),
                },
            ],
        },
        TestGroup {
            source: "chain_extensions/rand_extension",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 1 extension without a chain extension parent.
                    results: TestCaseResults::Diagnostic(1),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-type ErrorCode = RandomReadErr;"),
                        end_pat: Some("type ErrorCode = RandomReadErr;"),
                        replacement: "",
                    }]),
                    params: None,
                    // missing `ErrorCode` type.
                    results: TestCaseResults::Diagnostic(1),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "storage_items/default_storage_key_1",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        TestGroup {
            source: "storage_items/non_packed_tuple_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        TestGroup {
            source: "storage_items/complex_non_packed_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        TestGroup {
            source: "storage_items/complex_non_packed_enum",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        TestGroup {
            source: "storage_items/complex_packed_struct",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        TestGroup {
            source: "storage_items/complex_packed_enum",
            test_cases: vec![TestCase {
                modifications: None,
                params: None,
                results: TestCaseResults::Diagnostic(0),
            }],
        },
        // Trait definitions.
        TestGroup {
            source: "trait_definitions/erc20_trait",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 6 messages without a trait definition nor impl parent.
                    results: TestCaseResults::Diagnostic(6),
                },
                TestCase {
                    modifications: Some(
                        (1..=6)
                            .map(|_| TestCaseModification {
                                start_pat: Some("<-#[ink(message)]"),
                                end_pat: Some("#[ink(message)]"),
                                replacement: "",
                            })
                            .collect(),
                    ),
                    params: None,
                    // 1 trait level "missing message(s)", 6 method level "not a message" errors.
                    results: TestCaseResults::Diagnostic(7),
                },
            ],
        },
        TestGroup {
            source: "trait_definitions/flipper_trait",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: None,
                    results: TestCaseResults::Diagnostic(0),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "",
                    }]),
                    params: None,
                    // 2 messages without a trait definition nor impl parent.
                    results: TestCaseResults::Diagnostic(2),
                },
                TestCase {
                    modifications: Some(vec![
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                        TestCaseModification {
                            start_pat: Some("<-#[ink(message)]"),
                            end_pat: Some("#[ink(message)]"),
                            replacement: "",
                        },
                    ]),
                    params: None,
                    // 1 trait level "missing message(s)", 2 method level "not a message" errors.
                    results: TestCaseResults::Diagnostic(3),
                },
            ],
        },
    ] {
        // Gets the original source code.
        let original_code = test_utils::get_source_code(test_group.source);

        // Iterates over all test cases.
        for test_case in test_group.test_cases {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some(modifications) = test_case.modifications {
                test_utils::apply_test_modifications(&mut test_code, &modifications);
            }

            // Runs diagnostics.
            let results = Analysis::new(&test_code).diagnostics();

            // Verifies diagnostics results.
            let expected_results = match test_case.results {
                TestCaseResults::Diagnostic(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(results.len(), expected_results);
        }
    }
}
