//! Utilities for translating from LSP to ink! analyzer types.

use line_index::{LineCol, WideEncoding, WideLineCol};

use super::PositionTranslationContext;

/// Translates LSP position to ink! analyzer offset.
pub fn offset(
    position: lsp_types::Position,
    context: &PositionTranslationContext,
) -> Option<ink_analyzer::TextSize> {
    let line_col = if context.encoding == lsp_types::PositionEncodingKind::UTF16
        || context.encoding == lsp_types::PositionEncodingKind::UTF32
    {
        // Handles wide position encodings.
        let wide_line_col = WideLineCol {
            line: position.line,
            col: position.character,
        };
        let wide_encoding = if context.encoding == lsp_types::PositionEncodingKind::UTF32 {
            WideEncoding::Utf32
        } else {
            WideEncoding::Utf16
        };
        context.line_index.to_utf8(wide_encoding, wide_line_col)?
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
    (start <= end).then_some(ink_analyzer::TextRange::new(start, end))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::offset_position_encoding_fixture;
    use line_index::LineIndex;

    #[test]
    fn offset_works() {
        // Retrieves a fixture with text, and groups of ink! analyzer UTF-8 offsets and their equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
        let (text, offset_position_groups) = offset_position_encoding_fixture();

        // Iterates over all groups of ink! analyzer UTF-8 offsets and their equivalent UTF-8, UTF-16 and UTF-32 LSP positions.
        for offset_and_positions in offset_position_groups {
            // Composes test cases for each position encoding kind.
            for (encoding, position, expected_offset) in [
                (
                    lsp_types::PositionEncodingKind::UTF8,
                    offset_and_positions.position_utf8,
                    Some(offset_and_positions.offset_utf8),
                ),
                (
                    lsp_types::PositionEncodingKind::UTF16,
                    offset_and_positions.position_utf16,
                    Some(offset_and_positions.offset_utf8),
                ),
                (
                    lsp_types::PositionEncodingKind::UTF32,
                    offset_and_positions.position_utf32,
                    Some(offset_and_positions.offset_utf8),
                ),
            ] {
                let context = PositionTranslationContext {
                    encoding,
                    line_index: LineIndex::new(text),
                };

                // Verifies that the computed offset (based on the encoding) matches the expected offset.
                assert_eq!(offset(position, &context), expected_offset);
            }
        }
    }
}
