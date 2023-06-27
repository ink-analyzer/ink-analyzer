//! LSP request handlers.

use crate::memory::Memory;
use crate::translator;
use crate::translator::PositionTranslationContext;
use ink_analyzer::Analysis;
use line_index::LineIndex;

/// Handles completion request.
pub fn handle_completion(
    params: lsp_types::CompletionParams,
    memory: &mut Memory,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::CompletionResponse>> {
    // Gets document uri and retrieves document from memory.
    let id = params.text_document_position.text_document.uri.to_string();
    match memory.get(&id) {
        Some(doc) => {
            // Composes translation context.
            let translation_context = PositionTranslationContext {
                encoding: translator::position_encoding(client_capabilities),
                line_index: LineIndex::new(&doc.content),
            };

            // Converts LSP position to ink! analyzer offset.
            let offset = translator::from_lsp::offset(
                params.text_document_position.position,
                &translation_context,
            )
            .ok_or(anyhow::format_err!("Invalid offset."))?;

            // Computes ink! analyzer completions and translates them into an LSP completion list.
            let completion_items: Vec<lsp_types::CompletionItem> = Analysis::new(&doc.content)
                .completions(offset)
                .into_iter()
                .filter_map(|completion| {
                    translator::to_lsp::completion(completion, &translation_context)
                })
                .collect();

            // Compose LSP completion response.
            Ok((!completion_items.is_empty()).then_some(
                lsp_types::CompletionList {
                    is_incomplete: true,
                    items: completion_items,
                }
                .into(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles hover request.
pub fn handle_hover(
    params: lsp_types::HoverParams,
    memory: &mut Memory,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::Hover>> {
    // Gets document uri and retrieves document from memory.
    let id = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    match memory.get(&id) {
        Some(doc) => {
            // Composes translation context.
            let translation_context = PositionTranslationContext {
                encoding: translator::position_encoding(client_capabilities),
                line_index: LineIndex::new(&doc.content),
            };

            // Converts LSP position to ink! analyzer offset.
            let offset = translator::from_lsp::offset(
                params.text_document_position_params.position,
                &translation_context,
            )
            .ok_or(anyhow::format_err!("Invalid offset."))?;

            // Computes ink! analyzer hover content and translates it to an LSP hover.
            Ok(Analysis::new(&doc.content)
                .hover(ink_analyzer::TextRange::empty(offset))
                .and_then(|hover| translator::to_lsp::hover(hover, &translation_context)))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

/// Handles code action request.
pub fn handle_action(
    params: lsp_types::CodeActionParams,
    memory: &mut Memory,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<lsp_types::CodeActionResponse>> {
    // Gets document uri and retrieves document from memory.
    let uri = params.text_document.uri;
    let id = uri.to_string();
    match memory.get(&id) {
        Some(doc) => {
            // Composes translation context.
            let translation_context = PositionTranslationContext {
                encoding: translator::position_encoding(client_capabilities),
                line_index: LineIndex::new(&doc.content),
            };

            // Converts LSP range to ink! analyzer text range.
            let text_range = translator::from_lsp::text_range(params.range, &translation_context)
                .ok_or(anyhow::format_err!("Invalid range."))?;

            // Computes ink! analyzer actions and translates them to LSP code actions.
            Ok(Some(
                Analysis::new(&doc.content)
                    .actions(text_range.start())
                    .into_iter()
                    .filter_map(|action| {
                        translator::to_lsp::action(action, uri.clone(), &translation_context)
                            .map(|code_action| code_action.into())
                    })
                    .collect(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}
