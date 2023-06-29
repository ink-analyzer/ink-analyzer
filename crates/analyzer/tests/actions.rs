//! integration tests for ink! analyzer actions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils;
use test_utils::{
    TestCase, TestCaseModification, TestCaseParams, TestCaseResults, TestGroup,
    TestParamsOffsetOnly, TestResultTextRange,
};

// The high-level methodology for code/intent actions test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute code/intent actions for the modified source code and a specific offset position.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn actions_works() {
    // Iterates over all test case groups.
    for test_group in [
        // Contracts.
        TestGroup {
            // Reads source code from the `erc20.rs` contract in `test_data/contracts` directory.
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
                        offset_pat: Some("<-mod erc20"),
                    })),
                    // Describes the expected code/intent actions.
                    results: TestCaseResults::Action(vec![TestResultTextRange {
                        // Declares expected code/intent action text as `#[ink::contract]`, applied to the text range whose
                        // starting offset is the position at the beginning of the `mod erc20` substring and
                        // end offset is also the position at the beginning of the `mod erc20` substring.
                        text: "#[ink::contract]",
                        start_pat: Some("<-mod erc20"),
                        end_pat: Some("<-mod erc20"),
                    }]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink::contract]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "(env=)",
                            start_pat: Some("#[ink::contract"),
                            end_pat: Some("#[ink::contract"),
                        },
                        TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink::contract"),
                            end_pat: Some("#[ink::contract"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(storage)]"),
                        end_pat: Some("#[ink(storage)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("pub struct Erc20"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink(storage)]"),
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
                        offset_pat: Some("<-pub struct Transfer"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("/// Event emitted when a token transfer occurs."),
                            end_pat: Some("/// Event emitted when a token transfer occurs."),
                        },
                        TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("/// Event emitted when a token transfer occurs."),
                            end_pat: Some("/// Event emitted when a token transfer occurs."),
                        },
                        TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("/// Event emitted when a token transfer occurs."),
                            end_pat: Some("/// Event emitted when a token transfer occurs."),
                        },
                        TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("/// Event emitted when a token transfer occurs."),
                            end_pat: Some("/// Event emitted when a token transfer occurs."),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink(event)]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultTextRange {
                        text: ", anonymous",
                        start_pat: Some("#[ink(event"),
                        end_pat: Some("#[ink(event"),
                    }]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-pub fn new(total_supply: Balance)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::test]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink(constructor)]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink(default)]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink(payable)]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink(selector=)]",
                            start_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                            end_pat: Some(
                                "a new ERC-20 contract with the specified initial supply.",
                            ),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink(constructor)]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: ", default",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        },
                        TestResultTextRange {
                            text: ", payable",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        },
                        TestResultTextRange {
                            text: ", selector=",
                            start_pat: Some("#[ink(constructor"),
                            end_pat: Some("#[ink(constructor"),
                        },
                    ]),
                },
                TestCase {
                    modifications: Some(vec![TestCaseModification {
                        start_pat: Some("<-#[ink(message)]"),
                        end_pat: Some("#[ink(message)]"),
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-pub fn total_supply(&self)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::test]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                        TestResultTextRange {
                            text: "#[ink(constructor)]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                        TestResultTextRange {
                            text: "#[ink(default)]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                        TestResultTextRange {
                            text: "#[ink(message)]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                        TestResultTextRange {
                            text: "#[ink(payable)]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                        TestResultTextRange {
                            text: "#[ink(selector=)]",
                            start_pat: Some("/// Returns the total token supply."),
                            end_pat: Some("/// Returns the total token supply."),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink(message)]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: ", default",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        },
                        TestResultTextRange {
                            text: ", payable",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        },
                        TestResultTextRange {
                            text: ", selector=",
                            start_pat: Some("#[ink(message"),
                            end_pat: Some("#[ink(message"),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink::test]"),
                    })),
                    results: TestCaseResults::Action(vec![]),
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
                        offset_pat: Some("<-pub trait BaseErc20"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::chain_extension]",
                            start_pat: Some(
                                "Trait implemented by all ERC-20 respecting smart contracts.",
                            ),
                            end_pat: Some(
                                "Trait implemented by all ERC-20 respecting smart contracts.",
                            ),
                        },
                        TestResultTextRange {
                            text: "#[ink::trait_definition]",
                            start_pat: Some(
                                "Trait implemented by all ERC-20 respecting smart contracts.",
                            ),
                            end_pat: Some(
                                "Trait implemented by all ERC-20 respecting smart contracts.",
                            ),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink::trait_definition]"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "(keep_attr=)",
                            start_pat: Some("#[ink::trait_definition"),
                            end_pat: Some("#[ink::trait_definition"),
                        },
                        TestResultTextRange {
                            text: "(namespace=)",
                            start_pat: Some("#[ink::trait_definition"),
                            end_pat: Some("#[ink::trait_definition"),
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
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-pub trait Psp22Extension"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::chain_extension]",
                            start_pat: Some("<-pub trait Psp22Extension"),
                            end_pat: Some("<-pub trait Psp22Extension"),
                        },
                        TestResultTextRange {
                            text: "#[ink::trait_definition]",
                            start_pat: Some("<-pub trait Psp22Extension"),
                            end_pat: Some("<-pub trait Psp22Extension"),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink::chain_extension]"),
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
                        offset_pat: Some("<-fn token_name(asset_id: u32)"),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::test]",
                            start_pat: Some("<-fn token_name(asset_id: u32)"),
                            end_pat: Some("<-fn token_name(asset_id: u32)"),
                        },
                        TestResultTextRange {
                            text: "#[ink(extension=)]",
                            start_pat: Some("<-fn token_name(asset_id: u32)"),
                            end_pat: Some("<-fn token_name(asset_id: u32)"),
                        },
                        TestResultTextRange {
                            text: "#[ink(handle_status=)]",
                            start_pat: Some("<-fn token_name(asset_id: u32)"),
                            end_pat: Some("<-fn token_name(asset_id: u32)"),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink(extension = 0x3d26)]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultTextRange {
                        text: ", handle_status=",
                        start_pat: Some("#[ink(extension = 0x3d26"),
                        end_pat: Some("#[ink(extension = 0x3d26"),
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
                        replacement: "",
                    }]),
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-struct Contract("),
                    })),
                    results: TestCaseResults::Action(vec![
                        TestResultTextRange {
                            text: "#[ink::storage_item]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(anonymous)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(event)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                        TestResultTextRange {
                            text: "#[ink(storage)]",
                            start_pat: Some("#[derive(Default)]"),
                            end_pat: Some("#[derive(Default)]"),
                        },
                    ]),
                },
                TestCase {
                    modifications: None,
                    params: Some(TestCaseParams::Action(TestParamsOffsetOnly {
                        offset_pat: Some("<-#[ink::storage_item]"),
                    })),
                    results: TestCaseResults::Action(vec![TestResultTextRange {
                        text: "(derive=)",
                        start_pat: Some("#[ink::storage_item"),
                        end_pat: Some("#[ink::storage_item"),
                    }]),
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

            // Sets the cursor position.
            let offset_pat = match test_case.params.unwrap() {
                TestCaseParams::Action(it) => Some(it),
                _ => None,
            }
            .unwrap()
            .offset_pat;
            let offset =
                TextSize::from(test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32);

            // Computes actions.
            let results = Analysis::new(&test_code).actions(offset);

            // Verifies actions results.
            let expected_results = match test_case.results {
                TestCaseResults::Action(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results
                    .iter()
                    .map(|action| (action.edit.trim(), action.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|result| (
                        result.text,
                        TextRange::new(
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.start_pat).unwrap()
                                    as u32
                            ),
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.end_pat).unwrap()
                                    as u32
                            ),
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "source: {}",
                test_group.source
            );
        }
    }
}
