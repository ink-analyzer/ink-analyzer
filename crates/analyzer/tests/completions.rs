//! integration tests for ink! analyzer completions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils::parse_offset_at;

mod utils;

// The high-level methodology for completions test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/crates/analyzer/tests/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute completions for the modified source code and a specific offset position.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn completions_works() {
    for (source, test_cases) in [
        // Each item in this list has the following structure:
        // (source, [(rep_start_pat, rep_end_pat, replacement), (offset_pat, [(completion, comp_pat_start, comp_pat_end)])]) where:
        // source = location of the source code,
        // rep_start_pat = substring used to find the start offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // rep_end_pat = substring used to find the end offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // replacement = the replacement snippet that will inserted before tests are run on the modified source code,
        // offset_pat = substring used to find the cursor offset for the completion (see `test_utils::parse_offset_at` doc),
        // completion = the expected completion text,
        // comp_pat_start = substring used to find the start of the completion offset (see `test_utils::parse_offset_at` doc),
        // comp_pat_end = substring used to find the end of the completion offset (see `test_utils::parse_offset_at` doc).
        (
            // Reads source code from the `erc20.rs` contract in `test_data/contracts` directory.
            "contracts/erc20",
            // Defines test cases for the ink! entity file.
            vec![
                (
                    // Replaces `#[ink::contract]` with `#[ink]` in the source code.
                    (
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "#[ink]",
                    ),
                    (
                        // Set the offset position at the end of the `#[ink` substring.
                        Some("#[ink"),
                        // Describes the expected completions.
                        vec![
                            // Declares expected completion text as `ink::contract`, applied to the text range whose
                            // starting offset is the position at the beginning of the `ink]` substring and
                            // end offset is the position at the end of the `ink]` substring.
                            ("ink::contract", Some("<-ink]"), Some("#[ink")),
                        ],
                    ),
                ),
                (
                    (
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "#[ink::contract()]",
                    ),
                    (
                        Some("#[ink::contract("),
                        vec![
                            ("env=", Some("#[ink::contract("), Some("#[ink::contract(")),
                            (
                                "keep_attr=",
                                Some("#[ink::contract("),
                                Some("#[ink::contract("),
                            ),
                        ],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(storage)]"),
                        Some("#[ink(storage)]"),
                        "#[ink(s)]",
                    ),
                    (
                        Some("#[ink(s"),
                        vec![("storage", Some("<-s)]"), Some("#[ink(s"))],
                    ),
                ),
                (
                    (Some("<-#[ink(event)]"), Some("#[ink(event)]"), "#[ink(ev)]"),
                    (
                        Some("#[ink(ev"),
                        vec![("event", Some("<-ev)]"), Some("#[ink(ev"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(event)]"),
                        Some("#[ink(event)]"),
                        "#[ink(event,)]",
                    ),
                    (
                        Some("#[ink(event,"),
                        vec![("anonymous", Some("#[ink(event,"), Some("#[ink(event,"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(constructor)]"),
                        Some("#[ink(constructor)]"),
                        "#[ink(con)]",
                    ),
                    (
                        Some("#[ink(con"),
                        vec![("constructor", Some("<-con)]"), Some("#[ink(con"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(constructor)]"),
                        Some("#[ink(constructor)]"),
                        "#[ink(constructor,)]",
                    ),
                    (
                        Some("#[ink(constructor,"),
                        vec![
                            (
                                "default",
                                Some("#[ink(constructor,"),
                                Some("#[ink(constructor,"),
                            ),
                            (
                                "payable",
                                Some("#[ink(constructor,"),
                                Some("#[ink(constructor,"),
                            ),
                            (
                                "selector=",
                                Some("#[ink(constructor,"),
                                Some("#[ink(constructor,"),
                            ),
                        ],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(message)]"),
                        Some("#[ink(message)]"),
                        "#[ink(me)]",
                    ),
                    (
                        Some("#[ink(me"),
                        vec![("message", Some("<-me)]"), Some("#[ink(me"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(message)]"),
                        Some("#[ink(message)]"),
                        "#[ink(message,)]",
                    ),
                    (
                        Some("#[ink(message,"),
                        vec![
                            ("default", Some("#[ink(message,"), Some("#[ink(message,")),
                            ("payable", Some("#[ink(message,"), Some("#[ink(message,")),
                            ("selector=", Some("#[ink(message,"), Some("#[ink(message,")),
                        ],
                    ),
                ),
                (
                    (Some("<-#[ink::test]"), Some("#[ink::test]"), "#[ink::te]"),
                    (
                        Some("#[ink::te"),
                        vec![("test", Some("<-te]"), Some("#[ink::te"))],
                    ),
                ),
            ],
        ),
        (
            "trait_definitions/erc20_trait",
            vec![
                (
                    (
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        "#[ink::tr]",
                    ),
                    (
                        Some("#[ink::tr"),
                        vec![("trait_definition", Some("<-tr]"), Some("#[ink::tr"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        "#[ink::trait_definition()]",
                    ),
                    (
                        Some("#[ink::trait_definition("),
                        vec![
                            (
                                "keep_attr=",
                                Some("#[ink::trait_definition("),
                                Some("#[ink::trait_definition("),
                            ),
                            (
                                "namespace=",
                                Some("#[ink::trait_definition("),
                                Some("#[ink::trait_definition("),
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
                    (
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                        "#[ink]",
                    ),
                    (
                        Some("#[ink"),
                        vec![
                            ("ink::chain_extension", Some("<-ink]"), Some("#[ink")),
                            ("ink::trait_definition", Some("<-ink]"), Some("#[ink")),
                        ],
                    ),
                ),
                (
                    (
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                        "#[ink::chain_extension()]",
                    ),
                    (Some("#[ink::chain_extension("), vec![]),
                ),
                (
                    (
                        Some("<-#[ink(extension = 0x3d26)]"),
                        Some("#[ink(extension = 0x3d26)]"),
                        "#[ink()]",
                    ),
                    (
                        Some("#[ink("),
                        vec![
                            ("extension=", Some("#[ink("), Some("#[ink(")),
                            ("handle_status=", Some("#[ink("), Some("#[ink(")),
                        ],
                    ),
                ),
                (
                    (
                        Some("<-#[ink(extension = 0x3d26)]"),
                        Some("#[ink(extension = 0x3d26)]"),
                        "#[ink(extension = 0x3d26,)]",
                    ),
                    (
                        Some("#[ink(extension = 0x3d26,"),
                        vec![(
                            "handle_status=",
                            Some("#[ink(extension = 0x3d26,"),
                            Some("#[ink(extension = 0x3d26,"),
                        )],
                    ),
                ),
            ],
        ),
        (
            "storage_items/non_packed_tuple_struct",
            vec![
                (
                    (
                        Some("<-#[ink::storage_item]"),
                        Some("#[ink::storage_item]"),
                        "#[ink]",
                    ),
                    (
                        Some("#[ink"),
                        vec![("ink::storage_item", Some("<-ink]"), Some("#[ink"))],
                    ),
                ),
                (
                    (
                        Some("<-#[ink::storage_item]"),
                        Some("#[ink::storage_item]"),
                        "#[ink::storage_item()]",
                    ),
                    (
                        Some("#[ink::storage_item("),
                        vec![(
                            "derive=",
                            Some("#[ink::storage_item("),
                            Some("#[ink::storage_item("),
                        )],
                    ),
                ),
            ],
        ),
    ] {
        // Gets the original source code.
        let original_code = utils::get_source_code(source);

        for ((rep_start_pat, rep_end_pat, replacement), (offset_pat, expected_results)) in
            test_cases
        {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications.
            let start_offset = parse_offset_at(&test_code, rep_start_pat).unwrap();
            let end_offset = parse_offset_at(&test_code, rep_end_pat).unwrap();
            test_code.replace_range(start_offset..end_offset, replacement);

            // Sets the cursor position.
            let offset = TextSize::from(parse_offset_at(&test_code, offset_pat).unwrap() as u32);

            // Computes completions.
            let results = Analysis::new(&test_code).completions(offset);

            // Verifies results.
            assert_eq!(
                results
                    .iter()
                    .map(|completion| (completion.edit.as_str(), completion.range))
                    .collect::<Vec<(&str, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|(edit, pat_start, pat_end)| (
                        edit,
                        TextRange::new(
                            TextSize::from(parse_offset_at(&test_code, pat_start).unwrap() as u32),
                            TextSize::from(parse_offset_at(&test_code, pat_end).unwrap() as u32)
                        )
                    ))
                    .collect::<Vec<(&str, TextRange)>>(),
                "source: {}",
                source
            );
        }
    }
}
