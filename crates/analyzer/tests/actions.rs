//! integration tests for ink! analyzer actions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils;

// The high-level methodology for code/intent actions test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/crates/analyzer/tests/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute code/intent actions for the modified source code and a specific offset position.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn actions_works() {
    for (source, test_cases) in [
        // Each item in this list has the following structure:
        // (source, [Option<(rep_start_pat, rep_end_pat, replacement)>, (offset_pat, [(action, action_pat_start, action_pat_end)])]) where:
        // source = location of the source code,
        // rep_start_pat = substring used to find the start offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // rep_end_pat = substring used to find the end offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // replacement = the replacement snippet that will inserted before tests are run on the modified source code,
        // offset_pat = substring used to find the cursor offset for the completion (see `test_utils::parse_offset_at` doc),
        // action = the expected action text,
        // action_pat_start = substring used to find the start of the action offset (see `test_utils::parse_offset_at` doc),
        // action_pat_end = substring used to find the end of the action offset (see `test_utils::parse_offset_at` doc).
        (
            // Reads source code from the `erc20.rs` contract in `test_data/contracts` directory.
            "contracts/erc20",
            // Defines test cases for the ink! entity file.
            vec![
                (
                    // Removes `#[ink::contract]` from the source code.
                    Some((Some("<-#[ink::contract]"), Some("#[ink::contract]"), "")),
                    (
                        // Set the offset position at the beginning of the `mod erc20` substring.
                        Some("<-mod erc20"),
                        // Describes the expected code/intent actions.
                        vec![
                            // Declares expected code/intent action text as `#[ink::contract]`, applied to the text range whose
                            // starting offset is the position at the beginning of the `mod erc20` substring and
                            // end offset is also the position at the beginning of the `mod erc20` substring.
                            ("#[ink::contract]", Some("<-mod erc20"), Some("<-mod erc20")),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink::contract]"),
                        vec![
                            ("(env=)", Some("#[ink::contract"), Some("#[ink::contract")),
                            (
                                "(keep_attr=)",
                                Some("#[ink::contract"),
                                Some("#[ink::contract"),
                            ),
                        ],
                    ),
                ),
                (
                    Some((Some("<-#[ink(storage)]"), Some("#[ink(storage)]"), "")),
                    (
                        Some("pub struct Erc20"),
                        vec![
                            (
                                "#[ink::storage_item]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(anonymous)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(event)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(storage)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                        ],
                    ),
                ),
                (None, (Some("<-#[ink(storage)]"), vec![])),
                (
                    Some((Some("<-#[ink(event)]"), Some("#[ink(event)]"), "")),
                    (
                        Some("<-pub struct Transfer"),
                        vec![
                            (
                                "#[ink::storage_item]",
                                Some("/// Event emitted when a token transfer occurs."),
                                Some("/// Event emitted when a token transfer occurs."),
                            ),
                            (
                                "#[ink(anonymous)]",
                                Some("/// Event emitted when a token transfer occurs."),
                                Some("/// Event emitted when a token transfer occurs."),
                            ),
                            (
                                "#[ink(event)]",
                                Some("/// Event emitted when a token transfer occurs."),
                                Some("/// Event emitted when a token transfer occurs."),
                            ),
                            (
                                "#[ink(storage)]",
                                Some("/// Event emitted when a token transfer occurs."),
                                Some("/// Event emitted when a token transfer occurs."),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink(event)]"),
                        vec![(", anonymous", Some("#[ink(event"), Some("#[ink(event"))],
                    ),
                ),
                (
                    Some((
                        Some("<-#[ink(constructor)]"),
                        Some("#[ink(constructor)]"),
                        "",
                    )),
                    (
                        Some("<-pub fn new(total_supply: Balance)"),
                        vec![
                            (
                                "#[ink::test]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                            (
                                "#[ink(constructor)]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                            (
                                "#[ink(default)]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                            (
                                "#[ink(message)]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                            (
                                "#[ink(payable)]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                            (
                                "#[ink(selector=)]",
                                Some("a new ERC-20 contract with the specified initial supply."),
                                Some("a new ERC-20 contract with the specified initial supply."),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink(constructor)]"),
                        vec![
                            (
                                ", default",
                                Some("#[ink(constructor"),
                                Some("#[ink(constructor"),
                            ),
                            (
                                ", payable",
                                Some("#[ink(constructor"),
                                Some("#[ink(constructor"),
                            ),
                            (
                                ", selector=",
                                Some("#[ink(constructor"),
                                Some("#[ink(constructor"),
                            ),
                        ],
                    ),
                ),
                (
                    Some((Some("<-#[ink(message)]"), Some("#[ink(message)]"), "")),
                    (
                        Some("<-pub fn total_supply(&self)"),
                        vec![
                            (
                                "#[ink::test]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                            (
                                "#[ink(constructor)]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                            (
                                "#[ink(default)]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                            (
                                "#[ink(message)]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                            (
                                "#[ink(payable)]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                            (
                                "#[ink(selector=)]",
                                Some("/// Returns the total token supply."),
                                Some("/// Returns the total token supply."),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink(message)]"),
                        vec![
                            (", default", Some("#[ink(message"), Some("#[ink(message")),
                            (", payable", Some("#[ink(message"), Some("#[ink(message")),
                            (", selector=", Some("#[ink(message"), Some("#[ink(message")),
                        ],
                    ),
                ),
                (None, (Some("<-#[ink::test]"), vec![])),
            ],
        ),
        (
            "trait_definitions/erc20_trait",
            vec![
                (
                    Some((
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        "",
                    )),
                    (
                        Some("<-pub trait BaseErc20"),
                        vec![
                            (
                                "#[ink::chain_extension]",
                                Some("Trait implemented by all ERC-20 respecting smart contracts."),
                                Some("Trait implemented by all ERC-20 respecting smart contracts."),
                            ),
                            (
                                "#[ink::trait_definition]",
                                Some("Trait implemented by all ERC-20 respecting smart contracts."),
                                Some("Trait implemented by all ERC-20 respecting smart contracts."),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink::trait_definition]"),
                        vec![
                            (
                                "(keep_attr=)",
                                Some("#[ink::trait_definition"),
                                Some("#[ink::trait_definition"),
                            ),
                            (
                                "(namespace=)",
                                Some("#[ink::trait_definition"),
                                Some("#[ink::trait_definition"),
                            ),
                        ],
                    ),
                ),
            ],
        ),
        (
            "chain_extensions/psp22_extension",
            vec![
                (
                    Some((
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                        "",
                    )),
                    (
                        Some("<-pub trait Psp22Extension"),
                        vec![
                            (
                                "#[ink::chain_extension]",
                                Some("<-pub trait Psp22Extension"),
                                Some("<-pub trait Psp22Extension"),
                            ),
                            (
                                "#[ink::trait_definition]",
                                Some("<-pub trait Psp22Extension"),
                                Some("<-pub trait Psp22Extension"),
                            ),
                        ],
                    ),
                ),
                (None, (Some("<-#[ink::chain_extension]"), vec![])),
                (
                    Some((
                        Some("<-#[ink(extension = 0x3d26)]"),
                        Some("#[ink(extension = 0x3d26)]"),
                        "",
                    )),
                    (
                        Some("<-fn token_name(asset_id: u32)"),
                        vec![
                            (
                                "#[ink::test]",
                                Some("<-fn token_name(asset_id: u32)"),
                                Some("<-fn token_name(asset_id: u32)"),
                            ),
                            (
                                "#[ink(extension=)]",
                                Some("<-fn token_name(asset_id: u32)"),
                                Some("<-fn token_name(asset_id: u32)"),
                            ),
                            (
                                "#[ink(handle_status=)]",
                                Some("<-fn token_name(asset_id: u32)"),
                                Some("<-fn token_name(asset_id: u32)"),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink(extension = 0x3d26)]"),
                        vec![(
                            ", handle_status=",
                            Some("#[ink(extension = 0x3d26"),
                            Some("#[ink(extension = 0x3d26"),
                        )],
                    ),
                ),
            ],
        ),
        (
            "storage_items/non_packed_tuple_struct",
            vec![
                (
                    Some((
                        Some("<-#[ink::storage_item]"),
                        Some("#[ink::storage_item]"),
                        "",
                    )),
                    (
                        Some("<-struct Contract("),
                        vec![
                            (
                                "#[ink::storage_item]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(anonymous)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(event)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                            (
                                "#[ink(storage)]",
                                Some("#[derive(Default)]"),
                                Some("#[derive(Default)]"),
                            ),
                        ],
                    ),
                ),
                (
                    None,
                    (
                        Some("<-#[ink::storage_item]"),
                        vec![(
                            "(derive=)",
                            Some("#[ink::storage_item"),
                            Some("#[ink::storage_item"),
                        )],
                    ),
                ),
            ],
        ),
    ] {
        // Gets the original source code.
        let original_code = test_utils::get_source_code(source);

        for (modifications, (offset_pat, expected_results)) in test_cases {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some((rep_start_pat, rep_end_pat, replacement)) = modifications {
                let start_offset = test_utils::parse_offset_at(&test_code, rep_start_pat).unwrap();
                let end_offset = test_utils::parse_offset_at(&test_code, rep_end_pat).unwrap();
                test_code.replace_range(start_offset..end_offset, replacement);
            }

            // Sets the cursor position.
            let offset =
                TextSize::from(test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32);

            // Computes actions.
            let results = Analysis::new(&test_code).actions(offset);

            // Verifies actions results.
            assert_eq!(
                results
                    .iter()
                    .map(|action| (action.edit.trim(), action.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        edit,
                        TextRange::new(
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, pat_start).unwrap() as u32
                            ),
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, pat_end).unwrap() as u32
                            ),
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "source: {}",
                source
            );
        }
    }
}
