//! integration tests for ink! analyzer actions.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils::{TestCaseParams, TestCaseResults};

// The high-level methodology for code/intent actions test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/contracts/erc20.rs).
// - (Optionally) Make some modifications to the source code at a specific offset/text range to create a specific test case.
// - Compute code/intent actions for the modified source code and a specific offset position.
// - Verify that the actual results match the expected results.
// See inline comments for more details.
#[test]
fn actions_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::actions_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::actions_fixtures() {
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
            .pat;
            let offset =
                TextSize::from(test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32);
            let range = TextRange::new(offset, offset);

            // Computes actions.
            let results = Analysis::new(&test_code).actions(range);

            // Verifies actions results.
            let expected_results = match test_case.results {
                TestCaseResults::Action(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results
                    .into_iter()
                    .map(|action| (test_utils::remove_whitespace(action.edit), action.range))
                    .collect::<Vec<(String, TextRange)>>(),
                expected_results
                    .into_iter()
                    .map(|result| (
                        test_utils::remove_whitespace(result.text.to_string()),
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
                    .collect::<Vec<(String, TextRange)>>(),
                "source: {}",
                test_group.source
            );
        }
    }
}
