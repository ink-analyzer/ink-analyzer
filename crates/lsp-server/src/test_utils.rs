//! Test utilities for ink! LSP server.

#![cfg(test)]

use crate::memory::Memory;

/// Returns client capabilities with support for UTF-8 position encoding.
pub fn simple_client_config() -> lsp_types::ClientCapabilities {
    lsp_types::ClientCapabilities {
        general: Some(lsp_types::GeneralClientCapabilities {
            position_encodings: Some(vec![
                lsp_types::PositionEncodingKind::UTF8,
                lsp_types::PositionEncodingKind::UTF16,
            ]),
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// Adds a document to memory and returns it's uri.
pub fn document(content: String, memory: &mut Memory) -> lsp_types::Url {
    let uri = lsp_types::Url::from_file_path("/tmp/file.rs").unwrap();
    memory.insert(uri.to_string(), content, 0);
    uri
}
