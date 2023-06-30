//! Translates between ink! analyzer and LSP types.

use line_index::LineIndex;
use lsp_types::PositionEncodingKind;

pub mod from_lsp;
pub mod to_lsp;

/// Represents context information necessary to translate between an LSP position/range and ink! analyzer offset/text range.
pub struct PositionTranslationContext {
    pub encoding: PositionEncodingKind,
    pub line_index: LineIndex,
}
