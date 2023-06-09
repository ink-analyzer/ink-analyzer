//! Utilities for translating from ink! analyzer to LSP types.

use line_index::WideEncoding;
use std::collections::HashMap;

use crate::translator::PositionTranslationContext;

/// Translates ink! analyzer offset to LSP position.
pub fn position(
    offset: ink_analyzer::TextSize,
    context: &PositionTranslationContext,
) -> Option<lsp_types::Position> {
    context
        .line_index
        .try_line_col(offset)
        .and_then(|line_col| {
            if context.encoding == lsp_types::PositionEncodingKind::UTF16
                || context.encoding == lsp_types::PositionEncodingKind::UTF32
            {
                // Handles wide position encodings.
                let wide_encoding = if context.encoding == lsp_types::PositionEncodingKind::UTF32 {
                    WideEncoding::Utf32
                } else {
                    WideEncoding::Utf16
                };
                context
                    .line_index
                    .to_wide(wide_encoding, line_col)
                    .map(|wide_line_col| {
                        lsp_types::Position::new(wide_line_col.line, wide_line_col.col)
                    })
            } else {
                // Handles uft-8 position encoding.
                Some(lsp_types::Position::new(line_col.line, line_col.col))
            }
        })
}

/// Translates ink! analyzer text range to LSP range.
pub fn range(
    range: ink_analyzer::TextRange,
    context: &PositionTranslationContext,
) -> Option<lsp_types::Range> {
    position(range.start(), context)
        .zip(position(range.end(), context))
        .map(|(start, end)| lsp_types::Range::new(start, end))
}

/// Translates ink! analyzer diagnostic to LSP diagnostic.
pub fn diagnostic(
    diagnostic: ink_analyzer::Diagnostic,
    context: &PositionTranslationContext,
) -> Option<lsp_types::Diagnostic> {
    range(diagnostic.range, context).map(|range| lsp_types::Diagnostic {
        range,
        message: diagnostic.message,
        severity: Some(match diagnostic.severity {
            ink_analyzer::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
            ink_analyzer::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        }),
        ..Default::default()
    })
}

/// Translates ink! analyzer completion item to LSP completion item.
pub fn completion(
    completion: ink_analyzer::Completion,
    snippet_support: bool,
    context: &PositionTranslationContext,
) -> Option<lsp_types::CompletionItem> {
    range(completion.range, context).map(|range| lsp_types::CompletionItem {
        label: completion.label,
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        detail: completion.detail,
        insert_text_format: snippet_support.then_some(match completion.snippet.as_ref() {
            Some(_) => lsp_types::InsertTextFormat::SNIPPET,
            None => lsp_types::InsertTextFormat::PLAIN_TEXT,
        }),
        text_edit: Some(
            lsp_types::TextEdit {
                range,
                new_text: match (snippet_support, completion.snippet) {
                    (true, Some(snippet)) => snippet,
                    _ => completion.edit,
                },
            }
            .into(),
        ),
        ..Default::default()
    })
}

/// Translates ink! analyzer hover content to LSP hover content.
pub fn hover(
    hover: ink_analyzer::Hover,
    context: &PositionTranslationContext,
) -> Option<lsp_types::Hover> {
    range(hover.range, context).map(|range| lsp_types::Hover {
        contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::from_markdown(
            hover.content,
        )),
        range: Some(range),
    })
}

/// Translates ink! analyzer action content to LSP code action.
pub fn code_action(
    action: ink_analyzer::Action,
    uri: lsp_types::Url,
    context: &PositionTranslationContext,
) -> Option<lsp_types::CodeAction> {
    range(action.range, context).map(|range| lsp_types::CodeAction {
        title: action.label,
        kind: Some(lsp_types::CodeActionKind::REFACTOR_REWRITE),
        edit: Some(lsp_types::WorkspaceEdit {
            changes: Some(HashMap::from([(
                uri,
                vec![lsp_types::TextEdit {
                    range,
                    new_text: action.edit,
                }],
            )])),
            ..Default::default()
        }),
        ..Default::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::offset_position_encoding_fixture;
    use line_index::LineIndex;

    #[test]
    fn position_works() {
        // Retrieves a fixture with text, and groups of ink! analyzer UTF-8 offsets and their equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
        let (text, offset_position_groups) = offset_position_encoding_fixture();

        // Iterates over all groups of ink! analyzer UTF-8 offsets and their equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
        for offset_and_positions in offset_position_groups {
            // Composes test cases for each position encoding kind.
            for (encoding, expected_position) in [
                (
                    lsp_types::PositionEncodingKind::UTF8,
                    Some(offset_and_positions.position_utf8),
                ),
                (
                    lsp_types::PositionEncodingKind::UTF16,
                    Some(offset_and_positions.position_utf16),
                ),
                (
                    lsp_types::PositionEncodingKind::UTF32,
                    Some(offset_and_positions.position_utf32),
                ),
            ] {
                let context = PositionTranslationContext {
                    encoding,
                    line_index: LineIndex::new(text),
                };

                // Verifies that the computed position matches the expected position for the encoding.
                assert_eq!(
                    position(offset_and_positions.offset_utf8, &context),
                    expected_position
                );
            }
        }
    }
}
