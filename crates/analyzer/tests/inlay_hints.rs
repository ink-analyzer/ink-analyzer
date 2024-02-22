//! integration tests for ink! analyzer inlay hints.

use ink_analyzer::{Analysis, TextRange, TextSize, Version};
use test_utils::{TestCaseParams, TestCaseResults};

// The high-level methodology for inlay hints test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory
//   (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/contracts/erc20.rs).
// - (Optionally) Make some modifications to the source code at a specific offset/text range to create a specific test case.
// - Compute inlay hints for the modified source code and a specific text range.
// - Verify that the actual results match the expected results.
// See inline comments for more details.
#[test]
fn inlay_hints_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::inlay_hints_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::inlay_hints_fixtures() {
        // Gets the original source code.
        let original_code = test_utils::read_source_code(test_group.source);

        // Iterates over all test cases.
        for test_case in test_group.test_cases {
            // Creates a copy of test code for this test case.
            let mut test_code = original_code.clone();

            // Applies test case modifications (if any).
            if let Some(modifications) = test_case.modifications {
                test_utils::apply_test_modifications(&mut test_code, &modifications);
            }

            // Sets the range.
            let range_pats = match test_case.params.unwrap() {
                TestCaseParams::InlayHints(Some(it)) => Some((it.start_pat, it.end_pat)),
                _ => None,
            };
            let range = range_pats.map(|(range_start_pat, range_end_pat)| {
                TextRange::new(
                    TextSize::from(
                        test_utils::parse_offset_at(&test_code, range_start_pat).unwrap() as u32,
                    ),
                    TextSize::from(
                        test_utils::parse_offset_at(&test_code, range_end_pat).unwrap() as u32,
                    ),
                )
            });

            // Computes inlay hints.
            let results = Analysis::new(&test_code, Version::V4).inlay_hints(range);

            // Verifies actions results.
            let expected_results = match test_case.results {
                TestCaseResults::InlayHints(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results
                    .into_iter()
                    .map(|item| (item.label, item.position, item.range))
                    .collect::<Vec<(String, TextSize, TextRange)>>(),
                expected_results
                    .iter()
                    .map(|result| (
                        result.text.to_owned(),
                        TextSize::from(
                            test_utils::parse_offset_at(&test_code, result.pos_pat).unwrap() as u32
                        ),
                        TextRange::new(
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.range_start_pat)
                                    .unwrap() as u32
                            ),
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, result.range_end_pat)
                                    .unwrap() as u32
                            ),
                        )
                    ))
                    .collect::<Vec<(String, TextSize, TextRange)>>(),
                "source: {}",
                test_group.source
            );
        }
    }
}
