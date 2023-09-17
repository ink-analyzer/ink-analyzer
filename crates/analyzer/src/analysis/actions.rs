//! ink! attribute and entity code/intent actions.

use ink_analyzer_ir::syntax::{SyntaxNode, TextRange, TextSize};
use ink_analyzer_ir::{FromSyntax, InkAttribute, InkFile};
use itertools::Itertools;

use super::utils;
use crate::analysis::text_edit;
use crate::TextEdit;

mod attr;
pub mod entity;
mod item;

/// An ink! attribute code/intent action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    /// Label which identifies the action.
    pub label: String,
    /// The kind of the action (e.g quickfix or refactor).
    pub kind: ActionKind,
    /// Range where the action is activated.
    pub range: TextRange,
    /// Text edits that will performed by the action.
    pub edits: Vec<TextEdit>,
}

/// The kind of the action (e.g quickfix or refactor).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum ActionKind {
    QuickFix,
    Refactor,
}

/// Computes ink! attribute actions for the text range.
pub fn actions(file: &InkFile, range: TextRange) -> Vec<Action> {
    let mut results = Vec::new();

    // Compute AST item-based ink! attribute actions.
    item::actions(&mut results, file, range);

    // Compute ink! attribute actions based on focused ink! attribute.
    attr::actions(&mut results, file, range);

    results
        .into_iter()
        // Deduplicate by edits.
        .unique_by(|item| item.edits.clone())
        // Format edits.
        .map(|item| Action {
            edits: text_edit::format_edits(item.edits, file).collect(),
            ..item
        })
        .collect()
}

impl Action {
    /// Removes an ink! attribute.
    pub(crate) fn remove_attribute(attr: &InkAttribute) -> Self {
        Self {
            label: format!("Remove `{}` attribute.", attr.syntax()),
            kind: ActionKind::QuickFix,
            range: attr.syntax().text_range(),
            edits: vec![TextEdit::delete(attr.syntax().text_range())],
        }
    }

    /// Removes an item.
    pub(crate) fn remove_item(item: &SyntaxNode) -> Self {
        Self {
            label: "Remove item.".to_string(),
            kind: ActionKind::QuickFix,
            range: item.text_range(),
            edits: vec![TextEdit::delete(item.text_range())],
        }
    }

    /// Moves an item (i.e a syntax node) to a new location.
    pub(crate) fn move_item(
        item: &SyntaxNode,
        offset: TextSize,
        label: String,
        indent_option: Option<&str>,
    ) -> Self {
        Self::move_item_with_affixes(item, offset, label, indent_option, None, None)
    }

    /// Moves an item (i.e a syntax node) to a new location with affixes (i.e. prefixes and suffixes).
    pub(crate) fn move_item_with_affixes(
        item: &SyntaxNode,
        offset: TextSize,
        label: String,
        indent_option: Option<&str>,
        prefix_option: Option<&str>,
        suffix_option: Option<&str>,
    ) -> Self {
        // Gets the unindented insert text.
        // NOTE: removes item's top-level indenting (if any).
        let mut insert_text = utils::item_indenting(item).map_or(item.to_string(), |item_indent| {
            utils::reduce_indenting(item.to_string().as_str(), item_indent.as_str())
        });

        // Applies indenting based on insert location (if specified).
        if let Some(indent) = indent_option {
            insert_text = utils::apply_indenting(insert_text.as_str(), indent);
        }

        // Adds prefix (if any).
        if let Some(prefix) = prefix_option {
            insert_text = format!("{prefix}{insert_text}");
        }

        // Adds suffix (if any).
        if let Some(suffix) = suffix_option {
            insert_text = format!("{insert_text}{suffix}");
        }

        Self {
            label,
            kind: ActionKind::QuickFix,
            range: item.text_range(),
            edits: vec![
                // Insert a copy of the item at the specified offset.
                TextEdit::insert(insert_text, offset),
                // Delete the item from current location.
                TextEdit::delete(item.text_range()),
            ],
        }
    }
}
