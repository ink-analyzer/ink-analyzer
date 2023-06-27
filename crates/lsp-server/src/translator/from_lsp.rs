//! Utilities for translating from LSP to ink! analyzer types.

use line_index::{LineCol, WideEncoding, WideLineCol};

use crate::translator::PositionTranslationContext;

/// Translates LSP position to ink! analyzer offset.
pub fn offset(
    position: lsp_types::Position,
    context: &PositionTranslationContext,
) -> Option<ink_analyzer::TextSize> {
    let line_col = if context.encoding == lsp_types::PositionEncodingKind::UTF16
        || context.encoding == lsp_types::PositionEncodingKind::UTF32
    {
        // Handles wide position encodings.
        let line_col = WideLineCol {
            line: position.line,
            col: position.character,
        };
        let wide_encoding = if context.encoding == lsp_types::PositionEncodingKind::UTF32 {
            WideEncoding::Utf32
        } else {
            WideEncoding::Utf16
        };
        context.line_index.to_utf8(wide_encoding, line_col)?
    } else {
        // Handles uft-8 position encoding.
        LineCol {
            line: position.line,
            col: position.character,
        }
    };

    context.line_index.offset(line_col)
}

/// Translates LSP range to ink! analyzer text range.
pub fn text_range(
    range: lsp_types::Range,
    context: &PositionTranslationContext,
) -> Option<ink_analyzer::TextRange> {
    let start = offset(range.start, context)?;
    let end = offset(range.end, context)?;
    (start < end).then_some(ink_analyzer::TextRange::new(start, end))
}
