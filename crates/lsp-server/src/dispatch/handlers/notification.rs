//! LSP notification handlers.

use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
};

use crate::memory::Memory;

/// Handles `DidOpenTextDocument` notification.
pub fn handle_did_open_text_document(
    params: DidOpenTextDocumentParams,
    memory: &mut Memory,
) -> anyhow::Result<()> {
    // Creates document in memory but only if it's a rust file.
    if params.text_document.language_id == "rust" {
        memory.insert(
            params.text_document.uri.to_string(),
            params.text_document.text,
            params.text_document.version,
        );
    }

    Ok(())
}

/// Handles `DidChangeTextDocument` notification.
pub fn handle_did_change_text_document(
    params: DidChangeTextDocumentParams,
    memory: &mut Memory,
) -> anyhow::Result<()> {
    // Updates document in memory using the latest content change event (if any).
    if let Some(event) = params.content_changes.last() {
        // Server is currently configured to receive full document on update events,
        // so no incremental processing is necessary.
        memory.update(
            params.text_document.uri.as_ref(),
            event.text.clone(),
            params.text_document.version,
        );
    }

    Ok(())
}

/// Handles `DidCloseTextDocument` notification.
pub fn handle_did_close_text_document(
    params: DidCloseTextDocumentParams,
    memory: &mut Memory,
) -> anyhow::Result<()> {
    // Removes document from memory.
    memory.remove(params.text_document.uri.as_ref());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::Document;
    use crate::test_utils::document_uri;
    use lsp_types::{
        TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
        VersionedTextDocumentIdentifier,
    };

    #[test]
    fn handle_did_open_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document parameters.
        let uri = document_uri();
        let version = 0;
        let content = String::new();

        // Calls handler with parameters for an open rust file and verifies that the file is added to memory.
        let result = handle_did_open_text_document(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "rust".to_owned(),
                    version,
                    text: content.clone(),
                },
            },
            &mut memory,
        );
        assert!(result.is_ok());
        assert_eq!(
            memory.get(uri.as_ref()),
            Some(Document {
                content: content.clone(),
                version
            })
            .as_ref()
        );

        // Re-initializes memory, calls handler with parameters for an open NON-rust file and verifies that the file is NOT added to memory.
        memory = Memory::new();
        let result = handle_did_open_text_document(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "xyz".to_owned(),
                    version,
                    text: content,
                },
            },
            &mut memory,
        );
        assert!(result.is_ok());
        assert_eq!(memory.get(uri.as_ref()), None);
    }

    #[test]
    fn handle_did_change_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document_uri();
        memory.insert(uri.to_string(), String::new(), 0);

        // Creates document update test parameters.
        let updated_version = 1;
        let updated_content = "A".to_owned();

        // Calls handler.
        let result = handle_did_change_text_document(
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: updated_version,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: updated_content.clone(),
                }],
            },
            &mut memory,
        );

        // Verifies handler result and expected actions.
        assert!(result.is_ok());
        assert_eq!(
            memory.get(uri.as_ref()),
            Some(Document {
                content: updated_content,
                version: updated_version
            })
            .as_ref()
        );
    }

    #[test]
    fn handle_did_close_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = document_uri();
        memory.insert(uri.to_string(), String::new(), 0);

        // Calls handler.
        let result = handle_did_close_text_document(
            DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
            },
            &mut memory,
        );

        // Verifies handler result and expected actions.
        assert!(result.is_ok());
        assert_eq!(memory.get(uri.as_ref()), None);
    }
}
