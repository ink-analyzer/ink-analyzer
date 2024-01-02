//! Utilities for composing LSP requests and notifications.

use ink_analyzer::Analysis;
use line_index::LineIndex;

use crate::memory::Memory;
use crate::translator::PositionTranslationContext;
use crate::{translator, utils};

/// Composes `PublishDiagnostics` notification parameters for a set of documents with changes.
pub fn publish_diagnostics(
    uri: &lsp_types::Url,
    memory: &Memory,
    client_capabilities: &lsp_types::ClientCapabilities,
) -> anyhow::Result<lsp_types::PublishDiagnosticsParams> {
    // Computes diagnostics.
    let (diagnostics, version) = match memory.get(uri.as_str()) {
        // Computes diagnostics for document.
        Some(doc) => {
            // Composes translation context.
            let translation_context = PositionTranslationContext {
                encoding: utils::position_encoding(client_capabilities),
                line_index: LineIndex::new(&doc.content),
            };
            // Computes ink! analyzer diagnostics and translates them into an LSP diagnostics.
            (
                Analysis::new(&doc.content)
                    .diagnostics()
                    .into_iter()
                    .filter_map(|diagnostic| {
                        translator::to_lsp::diagnostic(diagnostic, &translation_context)
                    })
                    .collect(),
                Some(doc.version),
            )
        }
        // Clears diagnostics for missing documents.
        None => (Vec::new(), None),
    };

    // Composes `PublishDiagnostics` notification parameters.
    Ok(lsp_types::PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics,
        version,
    })
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

        // Composes `PublishDiagnostics` notification parameters for the changes and verifies the expected results.
        let result = publish_diagnostics(&uri, &mut memory, &client_capabilities);
        assert!(result.is_ok());
        let params = result.as_ref().unwrap();
        assert_eq!(params.uri, uri);
        // 3 Expected diagnostics for missing storage, constructor and message.
        assert_eq!(params.diagnostics.len(), 3);
    }
}
