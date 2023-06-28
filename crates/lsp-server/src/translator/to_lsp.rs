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
    context: &PositionTranslationContext,
) -> Option<lsp_types::CompletionItem> {
    range(completion.range, context).map(|range| lsp_types::CompletionItem {
        label: completion.label,
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        text_edit: Some(
            lsp_types::TextEdit {
                range,
                new_text: completion.edit,
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
        kind: Some(lsp_types::CodeActionKind::EMPTY),
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
