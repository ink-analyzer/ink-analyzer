//! integration tests for ink! analyzer completions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils;
use test_utils::{
    TestCase, TestCaseModification, TestCaseParams, TestCaseResults, TestGroup,
    TestParamsOffsetOnly, TestResultTextRange,
};

// The high-level methodology for completions test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute completions for the modified source code and a specific offset position.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn completions_works() {
    // Iterates over all test case groups.
    for test_group/*(source, test_cases)*/ in [
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test_data/contracts` directory.
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
                        offset_pat: Some("#[ink"),
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
                        offset_pat: Some("#[ink::contract("),
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
                        }
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "#[ink(s)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(s"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "storage",
                            start_pat: Some("<-s)]"),
                            end_pat: Some("#[ink(s"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "#[ink(ev)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(ev"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "event",
                            start_pat: Some("<-ev)]"),
                            end_pat: Some("#[ink(ev"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(event)]"),
                        end_pat: Some("#[ink(event)]"),
                        replacement: "#[ink(event,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(event,"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "anonymous",
                            start_pat: Some("#[ink(event,"),
                            end_pat: Some("#[ink(event,"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "#[ink(con)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(con"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "constructor",
                            start_pat: Some("<-con)]"),
                            end_pat: Some("#[ink(con"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "#[ink(constructor,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(constructor,"),
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
                        offset_pat: Some("#[ink(me"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-me)]"),
                            end_pat: Some("#[ink(me"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "#[ink(message,)]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink(message,"),
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
                        offset_pat: Some("#[ink::te"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "test",
                            start_pat: Some("<-te]"),
                            end_pat: Some("#[ink::te"),
                        },
                    ]),
                },
            ]
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
                        offset_pat: Some("#[ink::tr"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "trait_definition",
                            start_pat: Some("<-tr]"),
                            end_pat: Some("#[ink::tr"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::trait_definition]"),
                        end_pat: Some("#[ink::trait_definition]"),
                        replacement: "#[ink::trait_definition()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink::trait_definition("),
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
            ]
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
                        offset_pat: Some("#[ink"),
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
                        offset_pat: Some("#[ink::chain_extension("),
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
                        offset_pat: Some("#[ink("),
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
                        offset_pat: Some("#[ink(extension = 0x3d26,"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "handle_status=",
                            start_pat: Some("#[ink(extension = 0x3d26,"),
                            end_pat: Some("#[ink(extension = 0x3d26,"),
                        },
                    ]),
                },
            ]
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
                        offset_pat: Some("#[ink"),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "ink::storage_item",
                            start_pat: Some("<-ink]"),
                            end_pat: Some("#[ink"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink::storage_item]"),
                        end_pat: Some("#[ink::storage_item]"),
                        replacement: "#[ink::storage_item()]",
                    }]),
                    params: Some(TestCaseParams::Completion(TestParamsOffsetOnly {
                        offset_pat: Some("#[ink::storage_item("),
                    })),
                    results: TestCaseResults::Completion(vec![
                        TestResultTextRange {
                            text: "derive=",
                            start_pat: Some("#[ink::storage_item("),
                            end_pat: Some("#[ink::storage_item("),
                        },
                    ]),
                },
            ]
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

            // Sets the cursor position.
            let offset_pat = match test_case.params.unwrap() {
                TestCaseParams::Completion(it) => Some(it),
                _ => None,
            }
                .unwrap()
                .offset_pat;
            let offset =
                TextSize::from(test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32);

            // Computes completions.
            let results = Analysis::new(&test_code).completions(offset);

            // Verifies completion results.
            let expected_results = match test_case.results {
                TestCaseResults::Completion(it) => Some(it),
                _ => None,
            }
                .unwrap();
            assert_eq!(
                results
                    .iter()
                    .map(|completion| (completion.edit.as_str(), completion.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|result| (
                        result.text,
                        TextRange::new(
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.start_pat).unwrap() as u32
                            ),
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.end_pat).unwrap() as u32
                            )
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "source: {}",
                test_group.source
            );
        }
    }
}
