//! integration tests for ink! analyzer diagnostics.

use ink_analyzer::Analysis;
use test_utils::TestCaseResults;

// The high-level methodology for diagnostics test cases is:
// - Read the source code of an ink! entity file in the `test-fixtures` directory (e.g https://github.com/ink-analyzer/ink-analyzer/blob/master/test-fixtures/contracts/erc20.rs).
// - (Optionally) Make modifications to the source code at specific offsets/text ranges to create a specific test case.
// - Compute diagnostics for the modified source code.
// - Verify that the actual results match the expected results.
// See inline comments for more details.
#[test]
fn diagnostics_works() {
    // Iterates over all test case groups (see [`test_utils::fixtures::diagnostics_fixtures`] doc and inline comments).
    for test_group in test_utils::fixtures::diagnostics_fixtures() {
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

            // Runs diagnostics.
            let results = Analysis::new(&test_code).diagnostics();

            // Verifies diagnostics results.
            let expected_results = match test_case.results {
                TestCaseResults::Diagnostic(it) => Some(it),
                _ => None,
            }
            .unwrap();
            assert_eq!(results.len(), expected_results);
        }
    }
}
