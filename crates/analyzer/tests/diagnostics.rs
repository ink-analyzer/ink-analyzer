//! integration tests for ink! analyzer diagnostics.

mod utils;

use crate::utils::ink_version_from_path;
use ink_analyzer::{Analysis, TextRange, TextSize};
use test_utils::{PartialMatchStr, TestCaseResults};

// The high-level methodology for diagnostics test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory
//   (e.g. https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/v4/erc20.rs).
// - (Optionally) Make modifications to the source code at specific offsets/text ranges to create a specific test case.
// - Compute diagnostics for the modified source code.
// - Verify that the actual results match the expected results.
// See inline comments for more details.
#[test]
fn diagnostics_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::diagnostics_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::diagnostics_fixtures() {
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

            // Runs diagnostics.
            let version = ink_version_from_path(test_group.source);
            let results = Analysis::new(&test_code, version).diagnostics();

            // Verifies diagnostics results.
            let expected_results = match test_case.results {
                TestCaseResults::Diagnostic { n, quickfixes } => Some((n, quickfixes)),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results.len(),
                expected_results.0,
                "source: {}",
                test_group.source
            );
            // Verifies quickfixes.
            for (idx, result) in results.iter().enumerate() {
                assert_eq!(
                    result.quickfixes.as_ref().map(Vec::len),
                    expected_results
                        .1
                        .get(idx)
                        .map(|(expected_quickfixes, _)| expected_quickfixes.len()),
                    "source: {}",
                    test_group.source
                );
                let expected_quickfixes = &expected_results.1[idx].0;
                if let Some(quickfixes) = result.quickfixes.as_ref() {
                    assert_eq!(
                        quickfixes
                            .iter()
                            .map(|action| (
                                PartialMatchStr::from(action.label.as_str()),
                                action
                                    .edits
                                    .iter()
                                    .map(|edit| (
                                        PartialMatchStr::from(edit.text.as_str()),
                                        edit.range
                                    ))
                                    .collect()
                            ))
                            .collect::<Vec<(PartialMatchStr, Vec<(PartialMatchStr, TextRange)>)>>(),
                        expected_quickfixes
                            .iter()
                            .map(|expected_action| (
                                PartialMatchStr::from(expected_action.label),
                                expected_action
                                    .edits
                                    .iter()
                                    .map(|result| (
                                        PartialMatchStr::from(result.text),
                                        TextRange::new(
                                            TextSize::from(
                                                test_utils::parse_offset_at(
                                                    &test_code,
                                                    result.start_pat
                                                )
                                                .unwrap()
                                                    as u32
                                            ),
                                            TextSize::from(
                                                test_utils::parse_offset_at(
                                                    &test_code,
                                                    result.end_pat
                                                )
                                                .unwrap()
                                                    as u32
                                            ),
                                        )
                                    ))
                                    .collect()
                            ))
                            .collect::<Vec<(PartialMatchStr, Vec<(PartialMatchStr, TextRange)>)>>(),
                        "source: {}",
                        test_group.source
                    );
                }
            }
        }
    }
}
