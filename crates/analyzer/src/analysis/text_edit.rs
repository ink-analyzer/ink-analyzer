//! A text edit.

use ink_analyzer_ir::syntax::{TextRange, TextSize};

/// A text edit (with an optional snippet - i.e tab stops and/or placeholders).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    /// Replacement text for the text edit.
    pub text: String,
    /// Range to which the text edit will be applied.
    pub range: TextRange,
    /// Formatted snippet for the text edit (includes tab stops and/or placeholders).
    pub snippet: Option<String>,
}

impl TextEdit {
    /// Creates text edit.
    pub fn new(text: String, range: TextRange, snippet: Option<String>) -> Self {
        Self {
            text,
            range,
            snippet,
        }
    }

    /// Creates a text edit for an inserting at the given offset.
    pub fn insert(text: String, offset: TextSize, snippet: Option<String>) -> Self {
        Self {
            text,
            range: TextRange::new(offset, offset),
            snippet,
        }
    }

    /// Creates replacing the given range (alias of new).
    pub fn replace(text: String, range: TextRange, snippet: Option<String>) -> Self {
        Self::new(text, range, snippet)
    }

    /// Creates a text edit for deleting the specified range.
    pub fn delete(range: TextRange) -> Self {
        Self {
            text: "".to_string(),
            range,
            snippet: None,
        }
    }
}
