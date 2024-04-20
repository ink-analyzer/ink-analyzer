//! Utilities for translating from ink! analyzer to LSP types.

use std::collections::HashMap;

use line_index::WideEncoding;

use super::PositionTranslationContext;
use crate::utils::{SignatureSupport, COMMAND_EXTRACT_EVENT, COMMAND_MIGRATE_PROJECT};

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
        kind: Some(match completion.kind {
            ink_analyzer::CompletionKind::Attr | ink_analyzer::CompletionKind::Field => {
                lsp_types::CompletionItemKind::FIELD
            }
            ink_analyzer::CompletionKind::Enum => lsp_types::CompletionItemKind::ENUM,
            ink_analyzer::CompletionKind::Fn => lsp_types::CompletionItemKind::FUNCTION,
            ink_analyzer::CompletionKind::Mod => lsp_types::CompletionItemKind::MODULE,
            ink_analyzer::CompletionKind::Struct => lsp_types::CompletionItemKind::STRUCT,
            ink_analyzer::CompletionKind::Trait => lsp_types::CompletionItemKind::CLASS,
        }),
        detail: completion.detail,
        insert_text_format: snippet_support.then_some(match completion.edit.snippet {
            Some(_) => lsp_types::InsertTextFormat::SNIPPET,
            None => lsp_types::InsertTextFormat::PLAIN_TEXT,
        }),
        text_edit: Some(
            lsp_types::TextEdit {
                range,
                new_text: match (snippet_support, completion.edit.snippet) {
                    (true, Some(snippet)) => snippet,
                    _ => completion.edit.text,
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

/// Translates ink! analyzer action to LSP code action.
pub fn code_action(
    action: ink_analyzer::Action,
    uri: lsp_types::Url,
    edit_resolve_support: bool,
    context: &PositionTranslationContext,
) -> Option<lsp_types::CodeAction> {
    let is_migrate_trigger = action.kind == ink_analyzer::ActionKind::Migrate;
    let is_extract_trigger = action.kind == ink_analyzer::ActionKind::Extract;
    let edits: Vec<(&str, lsp_types::Range, Option<&str>)> = action
        .edits
        .iter()
        .filter_map(|edit| {
            range(edit.range, context)
                .map(|range| (edit.text.as_str(), range, edit.snippet.as_deref()))
        })
        .collect();
    (is_migrate_trigger || is_extract_trigger || !edits.is_empty()).then(|| {
        lsp_types::CodeAction {
            title: action.label.clone(),
            kind: Some(match action.kind {
                ink_analyzer::ActionKind::QuickFix => lsp_types::CodeActionKind::QUICKFIX,
                ink_analyzer::ActionKind::Refactor => lsp_types::CodeActionKind::REFACTOR_REWRITE,
                ink_analyzer::ActionKind::Migrate => lsp_types::CodeActionKind::REFACTOR_REWRITE,
                ink_analyzer::ActionKind::Extract => lsp_types::CodeActionKind::REFACTOR_EXTRACT,
                _ => lsp_types::CodeActionKind::EMPTY,
            }),
            edit: (!is_migrate_trigger && !is_extract_trigger && !edits.is_empty()).then_some(
                lsp_types::WorkspaceEdit {
                    changes: Some(HashMap::from([(
                        uri.clone(),
                        edits
                            .iter()
                            .map(|(text, range, _)| lsp_types::TextEdit {
                                range: *range,
                                new_text: text.to_string(),
                            })
                            .collect(),
                    )])),
                    ..Default::default()
                },
            ),
            data: if is_migrate_trigger || is_extract_trigger {
                // Add data for `codeAction/resolve`.
                edit_resolve_support.then_some(serde_json::json!({
                    "command": if is_migrate_trigger {
                        COMMAND_MIGRATE_PROJECT
                    } else {
                        COMMAND_EXTRACT_EVENT
                    }.to_owned(),
                    "uri": uri,
                    "range": serde_json::json!({
                        "start": u32::from(action.range.start()),
                        "end": u32::from(action.range.end()),
                    }),
                }))
            } else {
                // Add snippet for clients that have the middleware to apply code actions edits as snippets.
                let snippets: Vec<(String, String)> = edits
                    .into_iter()
                    .filter_map(|(text, _, snippet)| {
                        snippet.map(|snippet| (text.to_owned(), snippet.to_owned()))
                    })
                    .collect();
                (!snippets.is_empty()).then(|| {
                    let mut snippets_map = serde_json::Map::with_capacity(snippets.len());
                    for (edit, snippet) in snippets {
                        snippets_map.insert(edit, serde_json::Value::String(snippet));
                    }
                    let mut data = serde_json::Map::with_capacity(1);
                    data.insert(
                        "snippets".to_owned(),
                        serde_json::Value::Object(snippets_map),
                    );
                    serde_json::Value::Object(data)
                })
            },
            // Use `migrateProject` or `extractEvent` command for clients that can't resolve
            // the `edit` property via `codeAction/resolve`.
            command: if is_migrate_trigger || is_extract_trigger {
                (!edit_resolve_support).then_some(lsp_types::Command {
                    title: action.label,
                    command: if is_migrate_trigger {
                        COMMAND_MIGRATE_PROJECT
                    } else {
                        COMMAND_EXTRACT_EVENT
                    }
                    .to_owned(),
                    arguments: Some(vec![serde_json::json!({
                        "uri": uri,
                        "range": serde_json::json!({
                            "start": u32::from(action.range.start()),
                            "end": u32::from(action.range.end()),
                        }),
                    })]),
                })
            } else {
                None
            },
            ..Default::default()
        }
    })
}

/// Translates ink! analyzer inlay hint to LSP inlay hint.
pub fn inlay_hint(
    hint: ink_analyzer::InlayHint,
    context: &PositionTranslationContext,
) -> Option<lsp_types::InlayHint> {
    position(hint.position, context).map(|position| lsp_types::InlayHint {
        position,
        label: lsp_types::InlayHintLabel::String(format!(": {}", hint.label)),
        kind: Some(lsp_types::InlayHintKind::TYPE),
        text_edits: None,
        tooltip: hint.detail.map(lsp_types::InlayHintTooltip::String),
        padding_left: Some(true),
        padding_right: None,
        data: None,
    })
}

/// Translates ink! analyzer offset to LSP offset.
pub fn offset(
    offset: ink_analyzer::TextSize,
    encoding: &lsp_types::PositionEncodingKind,
    text: &str,
) -> u32 {
    if u32::from(offset) > 0
        && (*encoding == lsp_types::PositionEncodingKind::UTF16
            || *encoding == lsp_types::PositionEncodingKind::UTF32)
    {
        // Handles non-zero offsets for wide encodings (i.e. UTF-16 and UTF-32).
        let wide_encoding = if *encoding == lsp_types::PositionEncodingKind::UTF32 {
            WideEncoding::Utf32
        } else {
            WideEncoding::Utf16
        };
        let subject = &text[0..offset.into()];
        wide_encoding.measure(subject) as u32
    } else {
        // Handles other encodings (i.e. UTF-8) or the zero offset.
        offset.into()
    }
}

/// Translates ink! analyzer signature help to LSP signature help.
pub fn signature_help(
    signatures: &[ink_analyzer::SignatureHelp],
    signature_support: &SignatureSupport,
    prev_signature_help: Option<&lsp_types::SignatureHelp>,
    context: &PositionTranslationContext,
) -> Option<lsp_types::SignatureHelp> {
    // Determines the active signature based on the previous signature help (if any)
    // or defaults to the first signature.
    let active_signature = prev_signature_help
        .and_then(|prev_signatures| {
            prev_signatures
                .active_signature
                // Only retains the last active signature if it was user selected.
                .filter(|idx| *idx > 0)
                .and_then(|idx| prev_signatures.signatures.get(idx as usize))
        })
        .and_then(|prev_active_signature| {
            signatures
                .iter()
                .enumerate()
                .find(|(_, signature)| signature.label == prev_active_signature.label)
        })
        .or(signatures.first().map(|signature| (0, signature)));

    // Returns LSP signature help (if any).
    (!signatures.is_empty()).then(|| lsp_types::SignatureHelp {
        signatures: signatures
            .iter()
            .map(|signature| lsp_types::SignatureInformation {
                label: signature.label.clone(),
                documentation: signature.detail.as_ref().map(|doc| {
                    lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: doc.clone(),
                    })
                }),
                parameters: (!signature.parameters.is_empty()).then(|| {
                    signature
                        .parameters
                        .iter()
                        .map(|param| lsp_types::ParameterInformation {
                            label: if signature_support.label_offset_support {
                                lsp_types::ParameterLabel::LabelOffsets([
                                    offset(
                                        param.range.start(),
                                        &context.encoding,
                                        &signature.label,
                                    ),
                                    offset(param.range.end(), &context.encoding, &signature.label),
                                ])
                            } else {
                                let param_text = &signature.label
                                    [param.range.start().into()..param.range.end().into()];
                                lsp_types::ParameterLabel::Simple(param_text.to_owned())
                            },
                            documentation: param.detail.as_ref().map(|doc| {
                                lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                                    kind: lsp_types::MarkupKind::Markdown,
                                    value: doc.clone(),
                                })
                            }),
                        })
                        .collect()
                }),
                active_parameter: signature_support
                    .active_parameter_support
                    .then(|| signature.active_parameter.map(|idx| idx as u32))
                    .flatten(),
            })
            .collect(),
        active_signature: active_signature.map(|(idx, _)| idx as u32),
        active_parameter: active_signature
            .and_then(|(_, signature)| signature.active_parameter)
            .map(|idx| idx as u32),
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
