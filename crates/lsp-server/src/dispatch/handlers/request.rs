//! LSP request handlers.

use ink_analyzer::Analysis;
use line_index::LineIndex;

use crate::memory::Memory;
use crate::translator;
use crate::translator::PositionTranslationContext;

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
pub fn handle_code_action(
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
                        translator::to_lsp::code_action(action, uri.clone(), &translation_context)
                            .map(|code_action| code_action.into())
                    })
                    .collect(),
            ))
        }
        // Empty response for missing documents.
        None => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{document, simple_client_config};

    #[test]
    fn handle_completion_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document("#[ink::co]".to_string(), &mut memory);

        // Calls handler and verifies that the expected completion items are returned.
        let result = handle_completion(
            lsp_types::CompletionParams {
                text_document_position: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: lsp_types::Position {
                        line: 0,
                        character: 9,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
            &mut memory,
            &simple_client_config(),
        );
        assert!(result.is_ok());
        let completion_items = match result.unwrap().unwrap() {
            lsp_types::CompletionResponse::List(it) => Some(it),
            _ => None,
        }
        .unwrap()
        .items;
        assert!(completion_items[0].label.contains("ink! contract"));
    }

    #[test]
    fn handle_hover_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document("#[ink::contract]".to_string(), &mut memory);

        // Calls handler and verifies that the expected hover content is returned.
        let result = handle_hover(
            lsp_types::HoverParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri },
                    position: lsp_types::Position {
                        line: 0,
                        character: 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
            &mut memory,
            &simple_client_config(),
        );
        assert!(result.is_ok());
        let hover_content = match result.unwrap().unwrap().contents {
            lsp_types::HoverContents::Scalar(it) => match it {
                lsp_types::MarkedString::String(it) => Some(it),
                _ => None,
            },
            _ => None,
        }
        .unwrap();
        assert!(hover_content.contains("`#[ink::contract]`"));
    }

    #[test]
    fn handle_code_action_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document("mod my_contract {}".to_string(), &mut memory);

        // Calls handler and verifies that the expected code actions are returned.
        let result = handle_code_action(
            lsp_types::CodeActionParams {
                text_document: lsp_types::TextDocumentIdentifier { uri },
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 15,
                    },
                },
                context: Default::default(),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
            &mut memory,
            &simple_client_config(),
        );
        assert!(result.is_ok());
        let code_actions = result.unwrap().unwrap();
        assert!(match &code_actions[0] {
            lsp_types::CodeActionOrCommand::CodeAction(it) => Some(it),
            _ => None,
        }
        .unwrap()
        .title
        .contains("Add ink! contract"));
    }
}
