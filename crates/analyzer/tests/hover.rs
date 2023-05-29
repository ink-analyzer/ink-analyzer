//! integration tests for ink! analyzer actions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils::parse_offset_at;

mod utils;

#[test]
fn hover_works() {
    for (source, test_cases) in [
        // (source, [Option<(rep_start_pat, rep_end_pat, replacement)>, (range_start_pat, range_end_pat), Option<(match, match_pat_start, match_pat_end)>]) where:
        // source = location of the source code,
        // rep_start_pat = substring used to find the start offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // rep_end_pat = substring used to find the end offset for the replacement snippet (see `test_utils::parse_offset_at` doc),
        // replacement = the replacement snippet that will inserted before tests are run on the modified source code,
        // range_start_pat = substring used to find the start offset for the focus range (see `test_utils::parse_offset_at` doc),
        // range_end_pat = substring used to find the end offset for the focus range (see `test_utils::parse_offset_at` doc),
        // match = a substring that should exist in the hover content,
        // match_pat_start = substring used to find the start of the hover match offset (see `test_utils::parse_offset_at` doc),
        // match_pat_end = substring used to find the end of the hover match offset (see `test_utils::parse_offset_at` doc).
        (
            "contracts/erc20",
            vec![
                (
                    None,
                    (Some("<-#[ink::contract]"), Some("#[ink::contract]")),
                    Some((
                        "`#[ink::contract]`",
                        Some("<-contract]"),
                        Some("#[ink::contract"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        "#[ink::contract(env=MyEnvironment)]",
                    )),
                    (Some("#[ink::contract("), Some("#[ink::contract(env")),
                    Some((
                        "`#[ink::contract(env = E: impl Environment)]`",
                        Some("#[ink::contract("),
                        Some("#[ink::contract(env"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink::contract]"),
                        Some("#[ink::contract]"),
                        r#"#[ink::contract(keep_attr="foo,bar")]"#,
                    )),
                    (Some("#[ink::contract("), Some("#[ink::contract(keep_attr")),
                    Some((
                        "`#[ink::contract(keep_attr = N: string)]`",
                        Some("#[ink::contract("),
                        Some("#[ink::contract(keep_attr"),
                    )),
                ),
                (
                    None,
                    (Some("<-#[ink(storage)]"), Some("#[ink(storage)]")),
                    Some((
                        "`#[ink(storage)]`",
                        Some("<-storage)]"),
                        Some("#[ink(storage"),
                    )),
                ),
                (
                    None,
                    (Some("<-#[ink(event)]"), Some("#[ink(event)]")),
                    Some(("`#[ink(event)]`", Some("<-event)]"), Some("#[ink(event"))),
                ),
                (
                    Some((
                        Some("<-#[ink(event)]"),
                        Some("#[ink(event)]"),
                        "#[ink(event, anonymous)]",
                    )),
                    (Some("#[ink(event, "), Some("#[ink(event, anonymous")),
                    Some((
                        "`#[ink(anonymous)]`",
                        Some("#[ink(event, "),
                        Some("#[ink(event, anonymous"),
                    )),
                ),
                (
                    None,
                    (Some("<-#[ink(constructor)]"), Some("#[ink(constructor)]")),
                    Some((
                        "`#[ink(constructor)]`",
                        Some("<-constructor)]"),
                        Some("#[ink(constructor"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink(constructor)]"),
                        Some("#[ink(constructor)]"),
                        "#[ink(constructor, default)]",
                    )),
                    (
                        Some("#[ink(constructor, "),
                        Some("#[ink(constructor, default"),
                    ),
                    Some((
                        "`#[ink(default)]`",
                        Some("#[ink(constructor, "),
                        Some("#[ink(constructor, default"),
                    )),
                ),
                (
                    None,
                    (Some("<-#[ink(message)]"), Some("#[ink(message)]")),
                    Some((
                        "`#[ink(message)]`",
                        Some("<-message)]"),
                        Some("#[ink(message"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink(message)]"),
                        Some("#[ink(message)]"),
                        "#[ink(message, selector=_)]",
                    )),
                    (Some("#[ink(message, "), Some("#[ink(message, selector")),
                    Some((
                        "`#[ink(selector = S: u32 | _)]`",
                        Some("#[ink(message, "),
                        Some("#[ink(message, selector"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink(message)]"),
                        Some("#[ink(message)]"),
                        "#[ink(message, selector=1)]",
                    )),
                    (Some("#[ink(message, "), Some("#[ink(message, selector")),
                    Some((
                        "`#[ink(selector = S: u32 | _)]`",
                        Some("#[ink(message, "),
                        Some("#[ink(message, selector"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink(message)]"),
                        Some("#[ink(message)]"),
                        "#[ink(message, selector=0xA)]",
                    )),
                    (Some("#[ink(message, "), Some("#[ink(message, selector")),
                    Some((
                        "`#[ink(selector = S: u32 | _)]`",
                        Some("#[ink(message, "),
                        Some("#[ink(message, selector"),
                    )),
                ),
                (
                    None,
                    (Some("<-#[ink::test]"), Some("#[ink::test]")),
                    Some(("`#[ink::test]`", Some("<-test]"), Some("#[ink::test"))),
                ),
            ],
        ),
        (
            "trait_definitions/erc20_trait",
            vec![
                (
                    None,
                    (
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                    ),
                    Some((
                        "`#[ink::trait_definition]`",
                        Some("<-trait_definition]"),
                        Some("#[ink::trait_definition"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        r#"#[ink::trait_definition(keep_attr="foo,bar")]"#,
                    )),
                    (
                        Some("#[ink::trait_definition("),
                        Some("#[ink::trait_definition(keep_attr"),
                    ),
                    Some((
                        "`#[ink::trait_definition(keep_attr = N: string)]`",
                        Some("#[ink::trait_definition("),
                        Some("#[ink::trait_definition(keep_attr"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink::trait_definition]"),
                        Some("#[ink::trait_definition]"),
                        r#"#[ink::trait_definition(namespace="my_namespace")]"#,
                    )),
                    (
                        Some("#[ink::trait_definition("),
                        Some("#[ink::trait_definition(namespace"),
                    ),
                    Some((
                        "`#[ink::trait_definition(namespace = N: string)]`",
                        Some("#[ink::trait_definition("),
                        Some("#[ink::trait_definition(namespace"),
                    )),
                ),
            ],
        ),
        (
            "chain_extensions/psp22_extension",
            vec![
                (
                    None,
                    (
                        Some("<-#[ink::chain_extension]"),
                        Some("#[ink::chain_extension]"),
                    ),
                    Some((
                        "`#[ink::chain_extension]`",
                        Some("<-chain_extension]"),
                        Some("#[ink::chain_extension"),
                    )),
                ),
                (
                    None,
                    (
                        Some("<-#[ink(extension = 0x3d26)]"),
                        Some("#[ink(extension = 0x3d26)]"),
                    ),
                    Some((
                        "`#[ink(extension = N: u32)]`",
                        Some("<-extension = 0x3d26)]"),
                        Some("#[ink(extension"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink(extension = 0x3d26)]"),
                        Some("#[ink(extension = 0x3d26)]"),
                        "#[ink(extension = 0x3d26, handle_status=true)]",
                    )),
                    (
                        Some("#[ink(extension = 0x3d26, "),
                        Some("#[ink(extension = 0x3d26, handle_status"),
                    ),
                    Some((
                        "`#[ink(handle_status = flag: bool)]`",
                        Some("#[ink(extension = 0x3d26, "),
                        Some("#[ink(extension = 0x3d26, handle_status"),
                    )),
                ),
            ],
        ),
        (
            "storage_items/non_packed_tuple_struct",
            vec![
                (
                    None,
                    (Some("<-#[ink::storage_item]"), Some("#[ink::storage_item]")),
                    Some((
                        "`#[ink::storage_item]`",
                        Some("<-storage_item]"),
                        Some("#[ink::storage_item"),
                    )),
                ),
                (
                    Some((
                        Some("<-#[ink::storage_item]"),
                        Some("#[ink::storage_item]"),
                        r#"#[ink::storage_item(derive=false)]"#,
                    )),
                    (
                        Some("#[ink::storage_item("),
                        Some("#[ink::storage_item(derive"),
                    ),
                    Some((
                        "`#[ink::storage_item(derive = flag: bool)]`",
                        Some("#[ink::storage_item("),
                        Some("#[ink::storage_item(derive"),
                    )),
                ),
            ],
        ),
    ] {
        // Gets the original source code.
        let original_code = utils::get_source_code(source);

        for (modifications, (range_start_pat, range_end_pat), expected_results) in test_cases {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some((rep_start_pat, rep_end_pat, replacement)) = modifications {
                let start_offset = parse_offset_at(&test_code, rep_start_pat).unwrap();
                let end_offset = parse_offset_at(&test_code, rep_end_pat).unwrap();
                test_code.replace_range(start_offset..end_offset, replacement);
            }

            // Sets the focus range.
            let range = TextRange::new(
                TextSize::from(parse_offset_at(&test_code, range_start_pat).unwrap() as u32),
                TextSize::from(parse_offset_at(&test_code, range_end_pat).unwrap() as u32),
            );

            // Get hover content.
            let results = Analysis::new(&test_code).hover(range);

            // Verifies hover content.
            assert_eq!(
                results.is_some(),
                expected_results.is_some(),
                "source: {}",
                source
            );
            if results.is_some() {
                assert!(
                    results
                        .as_ref()
                        .unwrap()
                        .content
                        .contains(expected_results.unwrap().0),
                    "source: {}",
                    source
                );
            }
            assert_eq!(
                results.as_ref().map(|action| action.range),
                expected_results.map(|(_, pat_start, pat_end)| TextRange::new(
                    TextSize::from(parse_offset_at(&test_code, pat_start).unwrap() as u32),
                    TextSize::from(parse_offset_at(&test_code, pat_end).unwrap() as u32),
                )),
                "source: {}",
                source
            );
        }
    }
}
