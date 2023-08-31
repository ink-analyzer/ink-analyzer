//! Test utilities for IR types and abstractions.

#![cfg(test)]

use ink_analyzer_ir::syntax::{TextRange, TextSize};
use test_utils::{parse_offset_at, TestResultAction};

use crate::Action;

/// Verifies that code action results match the expected results description for a given code snippet.
pub fn verify_actions(
    code: &str,
    actual_results: &[Action],
    expected_results: &[TestResultAction],
) {
    // Verifies actions.
    assert_eq!(actual_results.len(), expected_results.len());
    for (idx, fix) in actual_results.iter().enumerate() {
        // Verifies action label.
        assert!(fix.label.contains(expected_results[idx].label));
        let expected_edits = &expected_results[idx].edits;
        // Verifies edits.
        assert_eq!(fix.edits.len(), expected_edits.len());
        for (idx, edit) in fix.edits.iter().enumerate() {
            // Verifies edit text.
            if expected_edits[idx].text.is_empty() {
                assert!(edit.text.is_empty());
            } else {
                assert!(edit.text.contains(expected_edits[idx].text));
            }
            // Verifies edit range.
            assert_eq!(
                edit.range,
                TextRange::new(
                    TextSize::from(
                        parse_offset_at(code, expected_edits[idx].start_pat).unwrap() as u32
                    ),
                    TextSize::from(
                        parse_offset_at(code, expected_edits[idx].end_pat).unwrap() as u32
                    )
                )
            );
        }
    }
}
