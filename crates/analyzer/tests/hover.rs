//! integration tests for ink! analyzer actions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils;
use test_utils::{
    TestCase, TestCaseModification, TestCaseParams, TestCaseResults, TestGroup,
    TestParamsRangeOnly, TestResultTextRange,
};

// The high-level methodology for hover content test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute hover content for the modified source code and a specific text range.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn hover_works() {
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
                    // Sets the text range for the hover to span the whole `#[ink::contract]` substring.
                    params: Some(TestCaseParams::Hover(TestParamsRangeOnly {
                        range_start_pat: Some("<-#[ink::contract]"),
                        range_end_pat: Some("#[ink::contract]"),
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
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(env"),
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
                        range_start_pat: Some("#[ink::contract("),
                        range_end_pat: Some("#[ink::contract(keep_attr"),
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
                        range_start_pat: Some("<-#[ink(storage)]"),
                        range_end_pat: Some("#[ink(storage)]"),
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
                        range_start_pat: Some("<-#[ink(event)]"),
                        range_end_pat: Some("#[ink(event)]"),
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
                        range_start_pat: Some("#[ink(event, "),
                        range_end_pat: Some("#[ink(event, anonymous"),
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
                        range_start_pat: Some("<-#[ink(constructor)]"),
                        range_end_pat: Some("#[ink(constructor)]"),
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
                        range_start_pat: Some("#[ink(constructor, "),
                        range_end_pat: Some("#[ink(constructor, default"),
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
                        range_start_pat: Some("<-#[ink(message)]"),
                        range_end_pat: Some("#[ink(message)]"),
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
                        range_start_pat: Some("#[ink(message, "),
                        range_end_pat: Some("#[ink(message, selector"),
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
                        range_start_pat: Some("#[ink(message, "),
                        range_end_pat: Some("#[ink(message, selector"),
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
                        range_start_pat: Some("#[ink(message, "),
                        range_end_pat: Some("#[ink(message, selector"),
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
                        range_start_pat: Some("<-#[ink::test]"),
                        range_end_pat: Some("#[ink::test]"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::test]`",
                        start_pat: Some("<-test]"),
                        end_pat: Some("#[ink::test"),
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
                        range_start_pat: Some("<-#[ink::trait_definition]"),
                        range_end_pat: Some("#[ink::trait_definition]"),
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
                        range_start_pat: Some("#[ink::trait_definition("),
                        range_end_pat: Some("#[ink::trait_definition(keep_attr"),
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
                        range_start_pat: Some("#[ink::trait_definition("),
                        range_end_pat: Some("#[ink::trait_definition(namespace"),
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
                        range_start_pat: Some("<-#[ink::chain_extension]"),
                        range_end_pat: Some("#[ink::chain_extension]"),
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
                        range_start_pat: Some("<-#[ink(extension = 0x3d26)]"),
                        range_end_pat: Some("#[ink(extension = 0x3d26)]"),
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
                        range_start_pat: Some("#[ink(extension = 0x3d26, "),
                        range_end_pat: Some("#[ink(extension = 0x3d26, handle_status"),
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
                        range_start_pat: Some("<-#[ink::storage_item]"),
                        range_end_pat: Some("#[ink::storage_item]"),
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
                        range_start_pat: Some("#[ink::storage_item("),
                        range_end_pat: Some("#[ink::storage_item(derive"),
                    })),
                    results: TestCaseResults::Hover(Some(TestResultTextRange {
                        text: "`#[ink::storage_item(derive = flag: bool)]`",
                        start_pat: Some("#[ink::storage_item("),
                        end_pat: Some("#[ink::storage_item(derive"),
                    })),
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

            // Sets the focus range.
            let (range_start_pat, range_end_pat) = match test_case.params.unwrap() {
                TestCaseParams::Hover(it) => Some((it.range_start_pat, it.range_end_pat)),
                _ => None,
            }
            .unwrap();
            let range = TextRange::new(
                TextSize::from(
                    test_utils::parse_offset_at(&test_code, range_start_pat).unwrap() as u32,
                ),
                TextSize::from(
                    test_utils::parse_offset_at(&test_code, range_end_pat).unwrap() as u32,
                ),
            );

            // Get hover content.
            let results = Analysis::new(&test_code).hover(range);

            // Verifies hover content.
            let expected_results = match test_case.results {
                TestCaseResults::Hover(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results.is_some(),
                expected_results.is_some(),
                "source: {}",
                test_group.source
            );
            if results.is_some() {
                assert!(
                    results
                        .as_ref()
                        .unwrap()
                        .content
                        .contains(expected_results.as_ref().unwrap().text),
                    "source: {}",
                    test_group.source
                );
            }
            assert_eq!(
                results.as_ref().map(|action| action.range),
                expected_results.map(|result| TextRange::new(
                    TextSize::from(
                        test_utils::parse_offset_at(&test_code, result.start_pat).unwrap() as u32
                    ),
                    TextSize::from(
                        test_utils::parse_offset_at(&test_code, result.end_pat).unwrap() as u32
                    ),
                )),
                "source: {}",
                test_group.source
            );
        }
    }
}
