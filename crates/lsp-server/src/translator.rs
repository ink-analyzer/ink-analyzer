//! Translates between ink! analyzer and LSP types.

pub mod from_lsp;
pub mod to_lsp;

use line_index::LineIndex;
use lsp_types::PositionEncodingKind;

/// Represents context information necessary to translate between an LSP position/range and ink! analyzer offset/text range.
pub struct PositionTranslationContext {
    pub encoding: PositionEncodingKind,
    pub line_index: LineIndex,
}

impl PositionTranslationContext {
    pub(crate) fn new(content: &str, encoding: PositionEncodingKind) -> Self {
        Self {
            encoding,
            line_index: LineIndex::new(content),
        }
    }
}
