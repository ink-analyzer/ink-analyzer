//! Test utilities for ink! LSP server.

#![cfg(test)]

use std::path::Path;
use std::str::FromStr;

use ink_analyzer::Version;

use crate::dispatch::{Snapshot, Snapshots};
use crate::utils;

/// Returns uri for a test document.
pub fn document_uri() -> lsp_types::Uri {
    lsp_types::Uri::from_str(
        url::Url::from_file_path(&Path::new(if cfg!(windows) {
            r#"C:\tmp\file.rs"#
        } else {
            "/tmp/file.rs"
        }))
        .unwrap()
        .as_str(),
    )
    .unwrap()
}

/// Initializes snapshots with a test document.
pub fn init_snapshots(
    content: String,
    client_capabilities: &lsp_types::ClientCapabilities,
    version: Version,
) -> (Snapshots, lsp_types::Uri) {
    let mut snapshots = Snapshots::new();
    let uri = document_uri();
    snapshots.insert(
        uri.to_string(),
        Snapshot::new(
            content,
            utils::position_encoding(client_capabilities),
            Some(0),
            version,
        ),
    );
    (snapshots, uri)
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
    // 🥺 - Pleading face/bottom emoji. | 4 (4) | 2 (4) | 1 (4)
    // Ref: <https://www.compart.com/en/unicode/U+1F97A>.

    (
        "A\n好\n𣎴\n🥺",
        vec![
            OffsetPositionGroup::new(1, 0, 1, 1, 1),
            OffsetPositionGroup::new(5, 1, 3, 1, 1),
            OffsetPositionGroup::new(10, 2, 4, 2, 1),
            OffsetPositionGroup::new(15, 3, 4, 2, 1),
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
