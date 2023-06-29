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

/// Returns uri for a test document.
pub fn document_uri() -> lsp_types::Url {
    lsp_types::Url::from_file_path("/tmp/file.rs").unwrap()
}

/// Adds a test document to memory and returns its uri.
pub fn document(content: String, memory: &mut Memory) -> lsp_types::Url {
    let uri = document_uri();
    memory.insert(uri.to_string(), content, 0);
    uri
}

// Returns a fixture with text, and groups of ink! analyzer UTF-8 offsets and their equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
pub fn offset_position_encoding_fixture() -> (&'static str, Vec<OffsetPositionGroup>) {
    // Ref: <https://www.unicode.org/faq/utf_bom.html>.
    // Ref: <https://www.w3.org/International/articles/definitions-characters/#charsets>.
    // Character - Description | UTF-8 code units (byte length) | UFT-16 code units (byte length) | UTF-32 code units (byte length)
    // A - Latin A. | 1 (1) | 1 (2) | 1 (4)
    // Ref: <https://www.compart.com/en/unicode/U+0041>.
    // 好 - Han ideograph AN. | 3 (3) | 1 (2) | 1 (4)
    // Ref: <https://www.compart.com/en/unicode/U+597D>.
    // 𣎴 - Chinese ideograph meaning 'stump of tree'. | 4 (4) | 2 (4) | 1 (4)
    // Ref: <https://www.compart.com/en/unicode/U+233B4>.

    (
        "A\n好\n𣎴",
        vec![
            OffsetPositionGroup::new(1, 0, 1, 1, 1),
            OffsetPositionGroup::new(5, 1, 3, 1, 1),
            OffsetPositionGroup::new(10, 2, 4, 2, 1),
        ],
    )
}

/// A group that consists of an ink! analyzer UTF-8 offset and its equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
pub struct OffsetPositionGroup {
    // The UTF-8 offset.
    pub offset_utf8: ink_analyzer::TextSize,
    // The UTF-8 position.
    pub position_utf8: lsp_types::Position,
    // The UTF-16 position.
    pub position_utf16: lsp_types::Position,
    // The UTF-32 position.
    pub position_utf32: lsp_types::Position,
}

impl OffsetPositionGroup {
    /// Creates new offset/position group.
    pub fn new(offset_utf8: u32, line: u32, col_utf8: u32, col_utf16: u32, col_utf32: u32) -> Self {
        Self {
            offset_utf8: ink_analyzer::TextSize::from(offset_utf8),
            position_utf8: lsp_types::Position {
                line,
                character: col_utf8,
            },
            position_utf16: lsp_types::Position {
                line,
                character: col_utf16,
            },
            position_utf32: lsp_types::Position {
                line,
                character: col_utf32,
            },
        }
    }
}
