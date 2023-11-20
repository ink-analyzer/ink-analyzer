//! Common traits for ink! entities.

use ra_ap_syntax::{AstNode, SyntaxNode, TextSize};

use crate::attrs::InkAttribute;
use crate::tree::{InkTree, ItemAtOffset};

/// Generic representation of an ink! entity (e.g. Contract, Storage, Event, Constructor e.t.c).
pub trait InkEntity {
    /// Associated AST node type.
    type AST: AstNode;

    /// Returns true if an ink! entity can be derived from the syntax node.
    ///
    /// The input syntax node can either be an attribute or an item.
    fn can_cast(node: &SyntaxNode) -> bool;

    /// Returns an ink! entity if one can be derived for the syntax node.
    ///
    /// The input syntax node can either be an attribute or an item.
    fn cast(node: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    /// Returns the root syntax node for the ink! entity.
    fn syntax(&self) -> &SyntaxNode;

    /// Returns the AST node for the ink! entity.
    fn ast(&self) -> Option<&Self::AST>;

    /// Returns the ink! attribute the ink! entity was derived from (if any).
    fn ink_attr(&self) -> Option<&InkAttribute>;

    /// Returns ink! entity tree.
    fn tree(&self) -> InkTree {
        InkTree::new(self.syntax())
    }

    /// Returns a representation of a token in the subtree which covers the position.
    fn item_at_offset(&self, offset: TextSize) -> ItemAtOffset {
        ItemAtOffset::new(self.syntax(), offset)
    }
}
