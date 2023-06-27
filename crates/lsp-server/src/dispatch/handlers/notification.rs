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
    // Creates document in memory.
    memory.insert(
        params.text_document.uri.to_string(),
        params.text_document.text,
        params.text_document.version,
    );

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
            event.text.to_owned(),
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
    use lsp_types::{
        TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Url,
        VersionedTextDocumentIdentifier,
    };

    #[test]
    fn handle_did_open_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test parameters.
        let uri = Url::from_file_path("/tmp/file.rs").unwrap();
        let version = 0;
        let content = "".to_string();

        // Calls handler.
        let result = handle_did_open_text_document(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "rust".to_string(),
                    version,
                    text: content.clone(),
                },
            },
            &mut memory,
        );

        // Verifies handler result and expected actions.
        assert!(result.is_ok());
        assert_eq!(
            memory.get(uri.as_ref()),
            Some(&Document { content, version })
        );
    }

    #[test]
    fn handle_did_change_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = Url::from_file_path("/tmp/file.rs").unwrap();
        memory.insert(uri.to_string(), "".to_string(), 0);

        // Creates update test parameters.
        let updated_version = 1;
        let updated_content = "A".to_string();

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
            Some(&Document {
                content: updated_content,
                version: updated_version
            })
        );
    }

    #[test]
    fn handle_did_close_text_document_works() {
        // Initializes memory.
        let mut memory = Memory::new();

        // Creates test document.
        let uri = Url::from_file_path("/tmp/file.rs").unwrap();
        memory.insert(uri.to_string(), "".to_string(), 0);

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
