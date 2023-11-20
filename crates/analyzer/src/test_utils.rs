//! Test utilities for IR types and abstractions.

#![cfg(test)]

use ink_analyzer_ir::syntax::{TextRange, TextSize};
use ink_analyzer_ir::{InkEntity, InkFile};
use test_utils::{parse_offset_at, PartialMatchStr, TestResultAction};

use crate::Action;

/// Verifies that code action results match the expected results description.
pub fn verify_actions(
    code: &str,
    actual_results: &[Action],
    expected_results: &[TestResultAction],
) {
    // Verifies actions.
    assert_eq!(actual_results.len(), expected_results.len(), "code: {code}");
    for (idx, fix) in actual_results.iter().enumerate() {
        // Verifies action label.
        assert_eq!(
            PartialMatchStr::from(fix.label.as_str()),
            PartialMatchStr::from(expected_results[idx].label),
            "code: {code}"
        );
        let expected_edits = &expected_results[idx].edits;
        // Verifies edits.
        assert_eq!(fix.edits.len(), expected_edits.len(), "code: {code}");
        for (idx, edit) in fix.edits.iter().enumerate() {
            // Verifies edit text.
            assert_eq!(
                PartialMatchStr::from(edit.text.as_str()),
                PartialMatchStr::from(expected_edits[idx].text),
                "code: {code}"
            );
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
                ),
                "code: {code}"
            );
        }
    }
}

/// Returns the first ink! entity of the generic type in the code snippet.
pub fn parse_first_ink_entity_of_type<T>(code: &str) -> T
where
    T: InkEntity,
{
    InkFile::parse(code)
        .syntax()
        .descendants()
        .find_map(T::cast)
        .unwrap()
}
