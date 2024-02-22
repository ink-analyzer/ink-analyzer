//! integration tests for ink! analyzer signature help.

use ink_analyzer::{Analysis, TextRange, TextSize, Version};
use test_utils::{TestCaseParams, TestCaseResults};

// The high-level methodology for signature help test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory
//   (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/contracts/erc20.rs).
// - (Optionally) Make some modifications to the source code at a specific offset/text range to create a specific test case.
// - Compute signature help for the modified source code and a specific offset position.
// - Verify that the actual results match the expected results.
// See inline comments for more details.
#[test]
fn signature_help_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::signature_help_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::signature_help_fixtures() {
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

            // Sets the cursor position.
            let offset_pat = match test_case.params.unwrap() {
                TestCaseParams::SignatureHelp(it) => Some(it),
                _ => None,
            }
            .unwrap()
            .pat;
            let offset =
                TextSize::from(test_utils::parse_offset_at(&test_code, offset_pat).unwrap() as u32);

            // Computes signature help.
            let results = Analysis::new(&test_code, Version::V4).signature_help(offset);

            // Verifies signature help results.
            let expected_results = match test_case.results {
                TestCaseResults::SignatureHelp(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(
                results
                    .into_iter()
                    .map(|signature_help| (
                        signature_help.label,
                        signature_help.range,
                        signature_help
                            .parameters
                            .iter()
                            .map(|param| param.range)
                            .collect(),
                        signature_help.active_parameter
                    ))
                    .collect::<Vec<(String, TextRange, Vec<TextRange>, Option<usize>)>>(),
                expected_results
                    .iter()
                    .map(|expected_result| (
                        expected_result.label.to_owned(),
                        TextRange::new(
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, expected_result.start_pat)
                                    .unwrap() as u32
                            ),
                            TextSize::from(
                                test_utils::parse_offset_at(&test_code, expected_result.end_pat)
                                    .unwrap() as u32
                            )
                        ),
                        expected_result
                            .params
                            .iter()
                            .map(|expected_param| {
                                TextRange::new(
                                    TextSize::from(
                                        test_utils::parse_offset_at(
                                            expected_result.label,
                                            expected_param.start_pat,
                                        )
                                        .unwrap() as u32,
                                    ),
                                    TextSize::from(
                                        test_utils::parse_offset_at(
                                            expected_result.label,
                                            expected_param.end_pat,
                                        )
                                        .unwrap() as u32,
                                    ),
                                )
                            })
                            .collect(),
                        expected_result.active_param
                    ))
                    .collect::<Vec<(String, TextRange, Vec<TextRange>, Option<usize>)>>(),
                "source: {}",
                test_group.source
            );
        }
    }
}
