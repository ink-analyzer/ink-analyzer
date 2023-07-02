//! integration tests for ink! analyzer hover content.

use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils::{TestCaseParams, TestCaseResults};

// The high-level methodology for hover content test cases is:
// - read the source code of an ink! entity file in the `test_data` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test_data/contracts/erc20.rs).
// - (optionally) make some modifications to the source code at a specific offset/text range to create a specific test case.
// - compute hover content for the modified source code and a specific text range.
// - verify that the actual results match the expected results.
// See inline comments for mode details.
#[test]
fn hover_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::hover_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::hover_fixtures() {
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
                results.as_ref().map(|hover| hover.range),
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
