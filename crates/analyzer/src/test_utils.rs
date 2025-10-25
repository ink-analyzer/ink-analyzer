//! Test utilities for IR types and abstractions.

#![cfg(test)]

use ink_analyzer_ir::syntax::{AstNode, Edition, SourceFile, TextRange, TextSize};
use ink_analyzer_ir::{InkEntity, InkFile};
use test_utils::{parse_offset_at, PartialMatchStr, TestResultAction};

use crate::{Action, TextEdit};

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

/// Returns the first AST node of the generic type in the code snippet.
pub fn parse_first_ast_node_of_type<T>(code: &str) -> T
where
    T: AstNode,
{
    parse_source(code)
        .syntax()
        .descendants()
        .find_map(T::cast)
        .unwrap()
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

macro_rules! versioned_fixtures {
    ($call: tt) => {
        [
            (crate::Version::Legacy, $call!(v4).collect::<Vec<_>>()),
            (
                crate::Version::V5(MinorVersion::Base),
                $call!(v5).collect::<Vec<_>>(),
            ),
            (crate::Version::V6, $call!(v6).collect::<Vec<_>>()),
        ]
    };
    (@legacy $call: tt) => {
        [
            (crate::Version::Legacy, $call!(v4).collect::<Vec<_>>()),
            (
                crate::Version::V5(MinorVersion::Base),
                $call!(v5).collect::<Vec<_>>(),
            ),
        ]
    };
}

pub fn text_edits_from_fixtures(
    code: &str,
    expected_results: Vec<(&str, Option<&str>, Option<&str>)>,
) -> Vec<TextEdit> {
    expected_results
        .into_iter()
        .map(|(text, start, end)| {
            TextEdit::new(
                text.to_owned(),
                TextRange::new(
                    TextSize::from(parse_offset_at(code, start).unwrap() as u32),
                    TextSize::from(parse_offset_at(code, end).unwrap() as u32),
                ),
                None,
            )
        })
        .collect()
}

/// Returns the `SourceFile` for the code snippet.
pub fn parse_source(code: &str) -> SourceFile {
    // TODO: Do we need an edition args?
    SourceFile::parse(code, Edition::Edition2021).tree()
}

macro_rules! prepend_migrate {
    ($version: expr, $list: expr) => {
        if $version.is_legacy() {
            vec![TestResultAction {
                label: "Migrate",
                edits: vec![],
            }]
        } else {
            vec![]
        }
        .into_iter()
        .chain($list)
        .collect::<Vec<TestResultAction>>()
    };
    ($list: expr) => {
        prepend_migrate!(Version::Legacy, $list)
    };
    () => {
        vec![TestResultAction {
            label: "Migrate",
            edits: vec![],
        }]
    };
}

macro_rules! chain_results {
    ($start: expr $(, $other: expr)+ $(,)?) => {
        $start.into_iter()$(.chain($other))*.collect::<Vec<TestResultAction>>()
    };
}
