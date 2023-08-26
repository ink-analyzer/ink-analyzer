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

    /// Creates text edit for inserting at the given offset.
    pub fn insert(text: String, offset: TextSize) -> Self {
        Self::insert_with_snippet(text, offset, None)
    }

    /// Creates text edit for inserting at the given offset (including an optional snippet).
    pub fn insert_with_snippet(text: String, offset: TextSize, snippet: Option<String>) -> Self {
        Self {
            text,
            range: TextRange::new(offset, offset),
            snippet,
        }
    }

    /// Creates text edit for replacing the given range.
    pub fn replace(text: String, range: TextRange) -> Self {
        Self::replace_with_snippet(text, range, None)
    }

    /// Creates text edit for replacing the given range (including an optional snippet) - i.e an alias of [`Self::new`].
    pub fn replace_with_snippet(text: String, range: TextRange, snippet: Option<String>) -> Self {
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
