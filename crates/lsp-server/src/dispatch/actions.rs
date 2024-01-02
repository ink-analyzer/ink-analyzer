//! Utilities for composing LSP requests and notifications.

use crate::dispatch::Snapshots;
use crate::translator;

/// Composes `PublishDiagnostics` notification parameters for a set of documents with changes.
pub fn publish_diagnostics(
    uri: &lsp_types::Url,
    snapshots: &Snapshots,
) -> anyhow::Result<lsp_types::PublishDiagnosticsParams> {
    // Computes diagnostics.
    let (diagnostics, version) = match snapshots.get(uri.as_str()) {
        // Computes diagnostics for document.
        Some(snapshot) => {
            // Computes ink! analyzer diagnostics and translates them into an LSP diagnostics.
            (
                snapshot
                    .analysis
                    .diagnostics()
                    .into_iter()
                    .filter_map(|diagnostic| {
                        translator::to_lsp::diagnostic(diagnostic, &snapshot.context)
                    })
                    .collect(),
                snapshot.version,
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
    use crate::dispatch::Snapshot;
    use crate::test_utils::document_uri;
    use crate::utils;
    use std::collections::HashMap;
    use test_utils::simple_client_config;

    #[test]
    fn publish_diagnostics_works() {
        // Creates client capabilities.
        let client_capabilities = simple_client_config();

        // Creates test document snapshot.
        let uri = document_uri();
        let mut snapshots = HashMap::new();
        snapshots.insert(
            uri.to_string(),
            Snapshot::new(
                String::from(
                    r#"
                        #[ink::contract]
                        mod my_contract {
                        }
                    "#,
                ),
                utils::position_encoding(&client_capabilities),
                Some(0),
            ),
        );

        // Composes `PublishDiagnostics` notification parameters for the changes and verifies the expected results.
        let result = publish_diagnostics(&uri, &snapshots);
        assert!(result.is_ok());
        let params = result.as_ref().unwrap();
        assert_eq!(params.uri, uri);
        // 3 Expected diagnostics for missing storage, constructor and message.
        assert_eq!(params.diagnostics.len(), 3);
    }
}
