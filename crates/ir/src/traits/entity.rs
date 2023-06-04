//! Generic tree traversal traits for ink! entities.

use ra_ap_syntax::TextSize;

use crate::tree::{InkTree, ItemAtOffset};
use crate::FromSyntax;

/// Convenience methods implemented by all ink! entities for traversing the ink! entity's syntax tree.
pub trait IsInkEntity {
    /// Returns ink! entity tree.
    fn tree(&self) -> InkTree;

    /// Returns a representation of a token in the subtree which covers the position.
    fn item_at_offset(&self, offset: TextSize) -> ItemAtOffset;
}

/// Blanket implementation of the InkItem for syntax node wrappers.
impl<T> IsInkEntity for T
where
    T: FromSyntax,
{
    fn tree(&self) -> InkTree {
        InkTree::new(self.syntax())
    }

    fn item_at_offset(&self, offset: TextSize) -> ItemAtOffset {
        ItemAtOffset::new(self.syntax(), offset)
    }
}
