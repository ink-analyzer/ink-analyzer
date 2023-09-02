//! Test fixtures for ink! analyzer.

use crate::{
    TestCase, TestCaseModification, TestCaseParams, TestCaseResults, TestGroup,
    TestParamsOffsetOnly, TestParamsRangeOnly, TestResultTextOffsetRange, TestResultTextRange,
};

/// Describes a collection of diagnostics tests to run against
/// optionally modified ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn diagnostics_fixtures() -> Vec<TestGroup> {
    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test-fixtures/contracts` directory.
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
    ]
}

/// Describes a collection of completions tests to run against
/// optionally modified ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn completions_fixtures() -> Vec<TestGroup> {
    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test-fixtures/contracts` directory.
            source: "contracts/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: vec![
                TestCase {
                    // Replaces `#[ink::contract]` with `#[ink]` in the source code.
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "#[ink]",
                    }]),
                    // Set the offset position at the end of the `#[ink` substring.
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    // Describes the expected completions.
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        // Declares expected completion text as `ink::contract`, applied to the text range whose
                        // starting offset is the position at the beginning of the `ink]` substring and
                        // end offset is the position at the end of the `ink]` substring.
                        text: "ink::contract",
                        start_pat: Some("<-ink]"),
                        end_pat: Some("#[ink"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "#[ink::contract()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::contract("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "env=",
                            start_pat: Some("#[ink::contract("),
                            end_pat: Some("#[ink::contract("),
                        },
                        TestResultTextRange {
                            text: "keep_attr=",
                            start_pat: Some("#[ink::contract("),
                            end_pat: Some("#[ink::contract("),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "#[ink(s)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(s"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "storage",
                        start_pat: Some("<-s)]"),
                        end_pat: Some("#[ink(s"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "#[ink(ev)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(ev"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "event",
                        start_pat: Some("<-ev)]"),
                        end_pat: Some("#[ink(ev"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "#[ink(event,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(event,"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "anonymous",
                        start_pat: Some("#[ink(event,"),
                        end_pat: Some("#[ink(event,"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "#[ink(con)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(con"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "constructor",
                        start_pat: Some("<-con)]"),
                        end_pat: Some("#[ink(con"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "#[ink(constructor,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(constructor,"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "default",
                            start_pat: Some("#[ink(constructor,"),
                            end_pat: Some("#[ink(constructor,"),
                        },
                        TestResultTextRange {
                            text: "payable",
                            start_pat: Some("#[ink(constructor,"),
                            end_pat: Some("#[ink(constructor,"),
                        },
                        TestResultTextRange {
                            text: "selector=",
                            start_pat: Some("#[ink(constructor,"),
                            end_pat: Some("#[ink(constructor,"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(me)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(me"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-me)]"),
                        end_pat: Some("#[ink(me"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(message,"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "default",
                            start_pat: Some("#[ink(message,"),
                            end_pat: Some("#[ink(message,"),
                        },
                        TestResultTextRange {
                            text: "payable",
                            start_pat: Some("#[ink(message,"),
                            end_pat: Some("#[ink(message,"),
                        },
                        TestResultTextRange {
                            text: "selector=",
                            start_pat: Some("#[ink(message,"),
                            end_pat: Some("#[ink(message,"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::test]"),
                        end_pat: Some("#[ink::test]"),
                        replacement: "#[ink::te]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::te"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "test",
                        start_pat: Some("<-te]"),
                        end_pat: Some("#[ink::te"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                        replacement: "#[ink_e2e::te]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink_e2e::te"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "test",
                        start_pat: Some("<-te]"),
                        end_pat: Some("#[ink_e2e::te"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                        replacement: "#[ink_e2e::test()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink_e2e::test("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "additional_contracts=",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test("),
                        },
                        TestResultTextRange {
                            text: "environment=",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test("),
                        },
                        TestResultTextRange {
                            text: "keep_attr=",
                            start_pat: Some("#[ink_e2e::test("),
                            end_pat: Some("#[ink_e2e::test("),
                        },
                    ]),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "trait_definitions/erc20_trait",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "#[ink::tr]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::tr"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "trait_definition",
                        start_pat: Some("<-tr]"),
                        end_pat: Some("#[ink::tr"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "#[ink::trait_definition()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::trait_definition("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "keep_attr=",
                            start_pat: Some("#[ink::trait_definition("),
                            end_pat: Some("#[ink::trait_definition("),
                        },
                        TestResultTextRange {
                            text: "namespace=",
                            start_pat: Some("#[ink::trait_definition("),
                            end_pat: Some("#[ink::trait_definition("),
                        },
                    ]),
                },
            ],
        },
        // Chain extensions.
        TestGroup {
            source: "chain_extensions/psp22_extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "ink::chain_extension",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                        TestResultTextRange {
                            text: "ink::trait_definition",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "#[ink::chain_extension()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::chain_extension("),
                    })),
                    results: TestCaseResults::Completion(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "extension=",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                        TestResultTextRange {
                            text: "handle_status=",
                            start_pat: Some("#[ink("),
                            end_pat: Some("#[ink("),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink(extension = 0x3d26,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink(extension = 0x3d26,"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "handle_status=",
                        start_pat: Some("#[ink(extension = 0x3d26,"),
                        end_pat: Some("#[ink(extension = 0x3d26,"),
                    }]),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "ink::storage_item",
                        start_pat: Some("<-ink]"),
                        end_pat: Some("#[ink"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink::storage_item()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        pat: Some("#[ink::storage_item("),
                    })),
                    results: TestCaseResults::Completion(vec![TestResultTextRange {
                        text: "derive=",
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item("),
                    }]),
                },
            ],
        },
    ]
}

/// Describes a collection of actions tests to run against
/// optionally modified ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn actions_fixtures() -> Vec<TestGroup> {
    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test-fixtures/contracts` directory.
            source: "contracts/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: vec![
                TestCase {
                    // Removes `#[ink::contract]` from the source code.
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "",
                    }]),
                    // Sets the offset position at the beginning of the `mod erc20` substring.
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-mod erc20"),
                    })),
                    // Describes the expected code/intent actions.
                    results: TestCaseResults::Action(vec![
                        // Declares expected code/intent action as one text edit,
                        // with text as `#[ink::contract]`, applied to the text range whose
                        // starting offset is the position at the beginning of the `mod erc20` substring and
                        // end offset is also the position at the beginning of the `mod erc20` substring.
                        vec![TestResultTextRange {
                            text: "#[ink::contract]",
                            start_pat: Some("<-mod erc20"),
                            end_pat: Some("<-mod erc20"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::contract]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "(env=)",
                            start_pat: Some("#[ink::contract"),
                            end_pat: Some("#[ink::contract"),
                        }],
                        vec![TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink::contract"),
                            end_pat: Some("#[ink::contract"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("pub struct Erc20"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("<-pub struct Erc20"),
                            end_pat: Some("<-pub struct Erc20"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("<-pub struct Erc20"),
                            end_pat: Some("<-pub struct Erc20"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("<-pub struct Erc20"),
                            end_pat: Some("<-pub struct Erc20"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("<-pub struct Erc20"),
                            end_pat: Some("<-pub struct Erc20"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(storage)]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub struct Transfer"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("<-pub struct Transfer"),
                            end_pat: Some("<-pub struct Transfer"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("<-pub struct Transfer"),
                            end_pat: Some("<-pub struct Transfer"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("<-pub struct Transfer"),
                            end_pat: Some("<-pub struct Transfer"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("<-pub struct Transfer"),
                            end_pat: Some("<-pub struct Transfer"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(event)]"),
                    })),
                    results: TestCaseResults::Action(vec![vec![TestResultTextRange {
                        text: ", anonymous",
                        start_pat: Some("#[ink(event"),
                        end_pat: Some("#[ink(event"),
                    }]]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub fn new(total_supply: Balance)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::test]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink_e2e::test]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(constructor)]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(default)]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(payable)]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(selector=)]",
                            start_pat: Some("<-pub fn new(total_supply: Balance)"),
                            end_pat: Some("<-pub fn new(total_supply: Balance)"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(constructor)]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: ", default",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        }],
                        vec![TestResultTextRange {
                            text: ", payable",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        }],
                        vec![TestResultTextRange {
                            text: ", selector=",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub fn total_supply(&self)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::test]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink_e2e::test]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(constructor)]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(default)]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(payable)]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(selector=)]",
                            start_pat: Some("<-pub fn total_supply(&self)"),
                            end_pat: Some("<-pub fn total_supply(&self)"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(message)]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: ", default",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        }],
                        vec![TestResultTextRange {
                            text: ", payable",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        }],
                        vec![TestResultTextRange {
                            text: ", selector=",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::test]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink_e2e::test]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "(additional_contracts=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                        vec![TestResultTextRange {
                            text: "(environment=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                        vec![TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-async fn e2e_transfer"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "(additional_contracts=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                        vec![TestResultTextRange {
                            text: "(environment=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                        vec![TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink_e2e::test"),
                            end_pat: Some("#[ink_e2e::test"),
                        }],
                    ]),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "trait_definitions/erc20_trait",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait BaseErc20"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::chain_extension]",
                            start_pat: Some("<-pub trait BaseErc20"),
                            end_pat: Some("<-pub trait BaseErc20"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink::trait_definition]",
                            start_pat: Some("<-pub trait BaseErc20"),
                            end_pat: Some("<-pub trait BaseErc20"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::trait_definition]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink::trait_definition"),
                            end_pat: Some("#[ink::trait_definition"),
                        }],
                        vec![TestResultTextRange {
                            text: "(namespace=)",
                            start_pat: Some("#[ink::trait_definition"),
                            end_pat: Some("#[ink::trait_definition"),
                        }],
                    ]),
                },
            ],
        },
        // Chain extensions.
        TestGroup {
            source: "chain_extensions/psp22_extension",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-pub trait Psp22Extension"),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::chain_extension]",
                            start_pat: Some("<-pub trait Psp22Extension"),
                            end_pat: Some("<-pub trait Psp22Extension"),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink::trait_definition]",
                            start_pat: Some("<-pub trait Psp22Extension"),
                            end_pat: Some("<-pub trait Psp22Extension"),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::chain_extension]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-fn token_name(asset_id: u32)"),
                    })),
                    results: TestCaseResults::Action(vec![vec![TestResultTextRange {
                        text: "#[ink(extension=1)]",
                        start_pat: Some("<-fn token_name(asset_id: u32)"),
                        end_pat: Some("<-fn token_name(asset_id: u32)"),
                    }]]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink(extension = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Action(vec![vec![TestResultTextRange {
                        text: ", handle_status=",
                        start_pat: Some("#[ink(extension = 0x3d26"),
                        end_pat: Some("#[ink(extension = 0x3d26"),
                    }]]),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-struct Contract("),
                    })),
                    results: TestCaseResults::Action(vec![
                        vec![TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("<-struct Contract("),
                            end_pat: Some("<-struct Contract("),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("<-struct Contract("),
                            end_pat: Some("<-struct Contract("),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("<-struct Contract("),
                            end_pat: Some("<-struct Contract("),
                        }],
                        vec![TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("<-struct Contract("),
                            end_pat: Some("<-struct Contract("),
                        }],
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        pat: Some("<-#[ink::storage_item]"),
                    })),
                    results: TestCaseResults::Action(vec![vec![TestResultTextRange {
                        text: "(derive=)",
                        start_pat: Some("#[ink::storage_item"),
                        end_pat: Some("#[ink::storage_item"),
                    }]]),
                },
            ],
        },
    ]
}

/// Describes a collection of hover content tests to run against
/// optionally modified ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn hover_fixtures() -> Vec<TestGroup> {
    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test-fixtures/contracts` directory.
            source: "contracts/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: vec![
                TestCase {
                    // Makes no modifications to the source code.
                    modifications: None,
                    // Sets the text range for the hover to span the whole `#[ink::contract]` substring.
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                    })),
                    // Describes the expected hover content.
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        // Expects the hover content to contain the substring "`#[ink::contract]`" and to highlight the text range whose
                        // starting offset is the position at the beginning of the `contract]` substring and
                        // end offset is the position at the end of the `#[ink::contract` substring.
                        text: "`#[ink::contract]`",
                        start_pat: Some("<-contract]"),
                        end_pat: Some("#[ink::contract"),
                    })),
                },
                TestCase {
                    // Replaces `#[ink::contract]` with `#[ink::contract(env=MyEnvironment)]` in the source code.
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: "#[ink::contract(env=MyEnvironment)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::contract("),
                        end_pat: Some("#[ink::contract(env"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::contract(env = E: impl Environment)]`",
                        start_pat: Some("#[ink::contract("),
                        end_pat: Some("#[ink::contract(env"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: r#"#[ink::contract(keep_attr="foo,bar")]"#,
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::contract("),
                        end_pat: Some("#[ink::contract(keep_attr"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::contract(keep_attr = N: string)]`",
                        start_pat: Some("#[ink::contract("),
                        end_pat: Some("#[ink::contract(keep_attr"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(storage)]`",
                        start_pat: Some("<-storage)]"),
                        end_pat: Some("#[ink(storage"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(event)]`",
                        start_pat: Some("<-event)]"),
                        end_pat: Some("#[ink(event"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "#[ink(event, anonymous)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(event, "),
                        end_pat: Some("#[ink(event, anonymous"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(anonymous)]`",
                        start_pat: Some("#[ink(event, "),
                        end_pat: Some("#[ink(event, anonymous"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(constructor)]`",
                        start_pat: Some("<-constructor)]"),
                        end_pat: Some("#[ink(constructor"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "#[ink(constructor, default)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(constructor, "),
                        end_pat: Some("#[ink(constructor, default"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(default)]`",
                        start_pat: Some("#[ink(constructor, "),
                        end_pat: Some("#[ink(constructor, default"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(message)]`",
                        start_pat: Some("<-message)]"),
                        end_pat: Some("#[ink(message"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message, selector=_)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(selector = S: u32 | _)]`",
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message, selector=1)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(selector = S: u32 | _)]`",
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message, selector=0xA)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(selector = S: u32 | _)]`",
                        start_pat: Some("#[ink(message, "),
                        end_pat: Some("#[ink(message, selector"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::test]"),
                        end_pat: Some("#[ink::test]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::test]`",
                        start_pat: Some("<-test]"),
                        end_pat: Some("#[ink::test"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink_e2e::test]`",
                        start_pat: Some("#[ink_e2e::"),
                        end_pat: Some("#[ink_e2e::test"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                        replacement: r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml")]"#,
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink_e2e::test("),
                        end_pat: Some("#[ink_e2e::test(additional_contracts"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink_e2e::test(additional_contracts = S: string)]`",
                        start_pat: Some("#[ink_e2e::test("),
                        end_pat: Some("#[ink_e2e::test(additional_contracts"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                        replacement: "#[ink_e2e::test(environment=MyEnvironment)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink_e2e::test("),
                        end_pat: Some("#[ink_e2e::test(environment"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink_e2e::test(environment = E: impl Environment)]`",
                        start_pat: Some("#[ink_e2e::test("),
                        end_pat: Some("#[ink_e2e::test(environment"),
                    })),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "trait_definitions/erc20_trait",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::trait_definition]`",
                        start_pat: Some("<-trait_definition]"),
                        end_pat: Some("#[ink::trait_definition"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: r#"#[ink::trait_definition(keep_attr="foo,bar")]"#,
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::trait_definition("),
                        end_pat: Some("#[ink::trait_definition(keep_attr"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::trait_definition(keep_attr = N: string)]`",
                        start_pat: Some("#[ink::trait_definition("),
                        end_pat: Some("#[ink::trait_definition(keep_attr"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: r#"#[ink::trait_definition(namespace="my_namespace")]"#,
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::trait_definition("),
                        end_pat: Some("#[ink::trait_definition(namespace"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::trait_definition(namespace = N: string)]`",
                        start_pat: Some("#[ink::trait_definition("),
                        end_pat: Some("#[ink::trait_definition(namespace"),
                    })),
                },
            ],
        },
        // Chain extensions.
        TestGroup {
            source: "chain_extensions/psp22_extension",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::chain_extension]"),
                        end_pat: Some("#[ink::chain_extension]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::chain_extension]`",
                        start_pat: Some("<-chain_extension]"),
                        end_pat: Some("#[ink::chain_extension"),
                    })),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(extension = N: u32)]`",
                        start_pat: Some("<-extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        end_pat: Some("#[ink(extension = 0x3d26)]"),
                        replacement: "#[ink(extension = 0x3d26, handle_status=true)]",
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink(extension = 0x3d26, "),
                        end_pat: Some("#[ink(extension = 0x3d26, handle_status"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink(handle_status = flag: bool)]`",
                        start_pat: Some("#[ink(extension = 0x3d26, "),
                        end_pat: Some("#[ink(extension = 0x3d26, handle_status"),
                    })),
                },
            ],
        },
        // Storage items.
        TestGroup {
            source: "storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::storage_item]`",
                        start_pat: Some("<-storage_item]"),
                        end_pat: Some("#[ink::storage_item"),
                    })),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: r#"#[ink::storage_item(derive=false)]"#,
                    }]),
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item(derive"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::storage_item(derive = flag: bool)]`",
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item(derive"),
                    })),
                },
            ],
        },
    ]
}

/// Describes a collection of inlay hints tests to run against
/// optionally modified ink! smart contract code in the `test-fixtures` directory in the project root.
pub fn inlay_hints_fixtures() -> Vec<TestGroup> {
    vec![
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test-fixtures/contracts` directory.
            source: "contracts/erc20",
            // Defines test cases for the ink! entity file.
            test_cases: vec![
                TestCase {
                    // Makes no modifications to the source code.
                    modifications: None,
                    // Sets the visible range for span the contents of the whole file.
                    // NOTE: `Some(TestCaseParams::InlayHints(None))` would also work in this case.
                    params: Some(TestCaseParams::InlayHints(Some(TestParamsRangeOnly {
                        start_pat: Some("<-"),
                        end_pat: Some("->"),
                    }))),
                    // Describes the expected results.
                    results: TestCaseResults::InlayHints(vec![]),
                },
                TestCase {
                    // Replaces `#[ink::contract]` with `#[ink::contract(env=MyEnvironment, keep_attr="foo,bar")]` in the source code.
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::contract]"),
                        end_pat: Some("#[ink::contract]"),
                        replacement: r#"#[ink::contract(env=MyEnvironment, keep_attr="foo,bar")]"#,
                    }]),
                    params: Some(TestCaseParams::InlayHints(None)),
                    // Expects the inlay hints for the `env` and `keep_attr` ink! attribute arguments
                    // positioned at the end of each of argument's name,
                    // with the argument's name being the text range the inlay hint applies to.
                    results: TestCaseResults::InlayHints(vec![
                        // env.
                        TestResultTextOffsetRange {
                            text: "impl Environment",
                            pos_pat: Some("#[ink::contract(env"),
                            range_start_pat: Some("#[ink::contract("),
                            range_end_pat: Some("#[ink::contract(env"),
                        },
                        // keep_attr.
                        TestResultTextOffsetRange {
                            text: "&str",
                            pos_pat: Some(r#"#[ink::contract(env=MyEnvironment, keep_attr"#),
                            range_start_pat: Some(r#"#[ink::contract(env=MyEnvironment, "#),
                            range_end_pat: Some(r#"#[ink::contract(env=MyEnvironment, keep_attr"#),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message, selector=_)]",
                    }]),
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![
                        // selector.
                        TestResultTextOffsetRange {
                            text: "u32 | _",
                            pos_pat: Some("<-=_"),
                            range_start_pat: Some("<-selector=_"),
                            range_end_pat: Some("<-=_"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink_e2e::test]"),
                        end_pat: Some("#[ink_e2e::test]"),
                        replacement: r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=MyEnvironment, keep_attr="foo,bar")]"#,
                    }]),
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![
                        // additional_contracts.
                        TestResultTextOffsetRange {
                            text: "&str",
                            pos_pat: Some("#[ink_e2e::test(additional_contracts"),
                            range_start_pat: Some("#[ink_e2e::test("),
                            range_end_pat: Some("#[ink_e2e::test(additional_contracts"),
                        },
                        // environment.
                        TestResultTextOffsetRange {
                            text: "impl Environment",
                            pos_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment"#,
                            ),
                            range_start_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", "#,
                            ),
                            range_end_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment"#,
                            ),
                        },
                        // keep_attr.
                        TestResultTextOffsetRange {
                            text: "&str",
                            pos_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=MyEnvironment, keep_attr"#,
                            ),
                            range_start_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=MyEnvironment, "#,
                            ),
                            range_end_pat: Some(
                                r#"#[ink_e2e::test(additional_contracts="adder/Cargo.toml flipper/Cargo.toml", environment=MyEnvironment, keep_attr"#,
                            ),
                        },
                    ]),
                },
            ],
        },
        // Trait definitions.
        TestGroup {
            source: "trait_definitions/erc20_trait",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: r#"#[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]"#,
                    }]),
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![
                        // namespace.
                        TestResultTextOffsetRange {
                            text: "&str",
                            pos_pat: Some("#[ink::trait_definition(namespace"),
                            range_start_pat: Some("#[ink::trait_definition("),
                            range_end_pat: Some("#[ink::trait_definition(namespace"),
                        },
                        // keep_attr.
                        TestResultTextOffsetRange {
                            text: "&str",
                            pos_pat: Some(
                                r#"#[ink::trait_definition(namespace="my_namespace", keep_attr"#,
                            ),
                            range_start_pat: Some(
                                r#"#[ink::trait_definition(namespace="my_namespace", "#,
                            ),
                            range_end_pat: Some(
                                r#"#[ink::trait_definition(namespace="my_namespace", keep_attr"#,
                            ),
                        },
                    ]),
                },
            ],
        },
        // Chain extensions.
        TestGroup {
            source: "chain_extensions/psp22_extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: Some(TestCaseParams::InlayHints(None)),
                results: TestCaseResults::InlayHints(vec![
                    // extensions.
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x3d26"),
                        range_start_pat: Some("<-extension = 0x3d26"),
                        range_end_pat: Some("<- = 0x3d26"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x3420"),
                        range_start_pat: Some("<-extension = 0x3420"),
                        range_end_pat: Some("<- = 0x3420"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x7271"),
                        range_start_pat: Some("<-extension = 0x7271"),
                        range_end_pat: Some("<- = 0x7271"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x162d"),
                        range_start_pat: Some("<-extension = 0x162d"),
                        range_end_pat: Some("<- = 0x162d"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x6568"),
                        range_start_pat: Some("<-extension = 0x6568"),
                        range_end_pat: Some("<- = 0x6568"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x4d47"),
                        range_start_pat: Some("<-extension = 0x4d47"),
                        range_end_pat: Some("<- = 0x4d47"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xdb20"),
                        range_start_pat: Some("<-extension = 0xdb20"),
                        range_end_pat: Some("<- = 0xdb20"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x54b3"),
                        range_start_pat: Some("<-extension = 0x54b3"),
                        range_end_pat: Some("<- = 0x54b3"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xb20f"),
                        range_start_pat: Some("<-extension = 0xb20f"),
                        range_end_pat: Some("<- = 0xb20f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0x96d6"),
                        range_start_pat: Some("<-extension = 0x96d6"),
                        range_end_pat: Some("<- = 0x96d6"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 0xfecb"),
                        range_start_pat: Some("<-extension = 0xfecb"),
                        range_end_pat: Some("<- = 0xfecb"),
                    },
                    // contract `env`.
                    TestResultTextOffsetRange {
                        text: "impl Environment",
                        pos_pat: Some("#[ink::contract(env"),
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(env"),
                    },
                    // selectors.
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x3d261bd4"),
                        range_start_pat: Some("<-selector = 0x3d261bd4"),
                        range_end_pat: Some("<- = 0x3d261bd4"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x34205be5"),
                        range_start_pat: Some("<-selector = 0x34205be5"),
                        range_end_pat: Some("<- = 0x34205be5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x7271b782"),
                        range_start_pat: Some("<-selector = 0x7271b782"),
                        range_end_pat: Some("<- = 0x7271b782"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x162df8c2"),
                        range_start_pat: Some("<-selector = 0x162df8c2"),
                        range_end_pat: Some("<- = 0x162df8c2"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x6568382f"),
                        range_start_pat: Some("<-selector = 0x6568382f"),
                        range_end_pat: Some("<- = 0x6568382f"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x4d47d921"),
                        range_start_pat: Some("<-selector = 0x4d47d921"),
                        range_end_pat: Some("<- = 0x4d47d921"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xdb20f9f5"),
                        range_start_pat: Some("<-selector = 0xdb20f9f5"),
                        range_end_pat: Some("<- = 0xdb20f9f5"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x54b3c76e"),
                        range_start_pat: Some("<-selector = 0x54b3c76e"),
                        range_end_pat: Some("<- = 0x54b3c76e"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xb20f1bbd"),
                        range_start_pat: Some("<-selector = 0xb20f1bbd"),
                        range_end_pat: Some("<- = 0xb20f1bbd"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0x96d6b57a"),
                        range_start_pat: Some("<-selector = 0x96d6b57a"),
                        range_end_pat: Some("<- = 0x96d6b57a"),
                    },
                    TestResultTextOffsetRange {
                        text: "u32 | _",
                        pos_pat: Some("<- = 0xfecb57d5"),
                        range_start_pat: Some("<-selector = 0xfecb57d5"),
                        range_end_pat: Some("<- = 0xfecb57d5"),
                    },
                ]),
            }],
        },
        TestGroup {
            source: "chain_extensions/rand_extension",
            test_cases: vec![TestCase {
                modifications: None,
                params: Some(TestCaseParams::InlayHints(None)),
                results: TestCaseResults::InlayHints(vec![
                    // extension.
                    TestResultTextOffsetRange {
                        text: "u32",
                        pos_pat: Some("<- = 1101"),
                        range_start_pat: Some("<-extension = 1101"),
                        range_end_pat: Some("<- = 1101"),
                    },
                    // env.
                    TestResultTextOffsetRange {
                        text: "impl Environment",
                        pos_pat: Some("#[ink::contract(env"),
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(env"),
                    },
                ]),
            }],
        },
        // Storage items.
        TestGroup {
            source: "storage_items/non_packed_tuple_struct",
            test_cases: vec![
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: r#"#[ink::storage_item(derive=false)]"#,
                    }]),
                    params: Some(TestCaseParams::InlayHints(None)),
                    results: TestCaseResults::InlayHints(vec![
                        // derive.
                        TestResultTextOffsetRange {
                            text: "bool",
                            pos_pat: Some("#[ink::storage_item(derive"),
                            range_start_pat: Some("#[ink::storage_item("),
                            range_end_pat: Some("#[ink::storage_item(derive"),
                        },
                    ]),
                },
            ],
        },
    ]
}
