//! Utilities for composing LSP requests and notifications.

use ink_analyzer::Analysis;
use line_index::LineIndex;
use std::collections::HashSet;

use crate::memory::Memory;
use crate::translator::PositionTranslationContext;
use crate::{translator, utils};

/// Composes `PublishDiagnostics` notification parameters for a set of documents with changes.
pub fn publish_diagnostics(
    changes: &HashSet<lsp_types::Url>,
    memory: &mut Memory,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<Option<Vec<lsp_types::PublishDiagnosticsParams>>> {
    // Iterates over all documents with changes and compose diagnostics parameters.
    let params: Vec<lsp_types::PublishDiagnosticsParams> = changes
        .iter()
        .map(|uri| {
            let (diagnostics, version, line_index) = match memory.get(uri.as_str()) {
                // Computes diagnostics for document.
                Some(doc) => (
                    Analysis::new(&doc.content).diagnostics(),
                    Some(doc.version),
                    Some(LineIndex::new(&doc.content)),
                ),
                // Clears diagnostics for missing documents.
                None => (Vec::new(), None, None),
            };

            // Composes translation context.
            let translation_context = line_index.map(|line_index| PositionTranslationContext {
                encoding: utils::position_encoding(client_capabilities),
                line_index,
            });

            // Translate ink! analyzer diagnostics to LSP diagnostics and composes `PublishDiagnostics` notification parameters.
            lsp_types::PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: diagnostics
                    .into_iter()
                    .filter_map(|diagnostic| {
                        translation_context
                            .as_ref()
                            .and_then(|context| translator::to_lsp::diagnostic(diagnostic, context))
                    })
                    .collect(),
                version,
            }
        })
        .collect();

    Ok((!params.is_empty()).then_some(params))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::document;
    use test_utils::simple_client_config;

    #[test]
    fn publish_diagnostics_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Creates test document.
        let uri = document(
            r#"
            #[ink::contract]
            mod my_contract {
            }
        "#
            .to_string(),
            &mut memory,
        );

        // Retrieves changes and convert ids to LSP URIs.
        let changes = memory
            .take_changes()
            .unwrap()
            .iter()
            .filter_map(|id| lsp_types::Url::parse(id).ok())
            .collect();

        // Composes `PublishDiagnostics` notification parameters for the changes and verifies the expected results.
        let result = publish_diagnostics(&changes, &mut memory, &client_capabilities);
        assert!(result.is_ok());
        assert!(result.as_ref().unwrap().is_some());
        let params = &result.as_ref().unwrap().as_ref().unwrap()[0];
        assert_eq!(&params.uri, &uri);
        // 3 Expected diagnostics for missing storage, constructor and message.
        assert_eq!(params.diagnostics.len(), 3);
    }
}
