//! Syntax tree traversal traits for ink! entities.

use ra_ap_syntax::SyntaxNode;

use crate::{utils, FromSyntax, InkArg, InkArgKind, InkAttribute};

/// Convenience methods for implemented by all ink! entities traversing the ink! entity's syntax tree.
pub trait InkEntity {
    /// Returns ink! entity tree.
    fn tree(&self) -> InkTree;
}

/// Blanket implementation of the InkItem for syntax node wrappers.
impl<T> InkEntity for T
where
    T: FromSyntax,
{
    fn tree(&self) -> InkTree {
        InkTree::new(self.syntax())
    }
}

/// A wrapper for ink! entity tree navigation methods that return opaque iterator types.
pub struct InkTree<'a> {
    /// The wrapped syntax node.
    syntax: &'a SyntaxNode,
}

impl<'a> InkTree<'a> {
    /// Converts the syntax node to an ink! entity tree.
    pub fn new(node: &'a SyntaxNode) -> Self {
        Self { syntax: node }
    }

    /// Returns ink! attributes for the ink! entity.
    pub fn ink_attrs(&self) -> impl Iterator<Item = InkAttribute> {
        utils::ink_attrs(self.syntax)
    }

    /// Returns ink! attributes for all the ink! entity's descendants.
    pub fn ink_attrs_descendants(&self) -> impl Iterator<Item = InkAttribute> {
        utils::ink_attrs_descendants(self.syntax)
    }

    /// Returns ink! attributes for all the ink! entity's descendants
    /// that don't have any ink! ancestors between them and the entity.
    pub fn ink_attrs_closest_descendants(&self) -> impl Iterator<Item = InkAttribute> {
        utils::ink_attrs_closest_descendants(self.syntax)
    }

    /// Returns ink! attributes in the ink! entity's scope.
    /// This includes both the ink! entity's own ink! attributes and those of all of it's descendants.
    pub fn ink_attrs_in_scope(&self) -> impl Iterator<Item = InkAttribute> {
        utils::ink_attrs_in_scope(self.syntax)
    }

    /// Returns ink! attributes for all the ink! entity's ancestors.
    pub fn ink_attrs_ancestors(&self) -> impl Iterator<Item = InkAttribute> + '_ {
        utils::ink_attrs_ancestors(self.syntax)
    }

    /// Returns ink! attributes for all the ink! entity's ancestors
    /// that don't have any ink! ancestors between them and the item.
    pub fn ink_attrs_closest_ancestors(&self) -> impl Iterator<Item = InkAttribute> {
        utils::ink_attrs_closest_ancestors(self.syntax)
    }

    /// Returns ink! arguments of the ink! entity.
    pub fn ink_args(&self) -> impl Iterator<Item = InkArg> {
        utils::ink_args(self.syntax)
    }

    /// Returns ink! arguments of a specific kind (if any) for the ink! entity.
    pub fn ink_args_by_kind(&self, kind: InkArgKind) -> impl Iterator<Item = InkArg> {
        utils::ink_args_by_kind(self.syntax, kind)
    }

    /// Returns ink! argument of a specific kind (if any) for the ink! entity.
    pub fn ink_arg_by_kind(&self, kind: InkArgKind) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax, kind)
    }
}
