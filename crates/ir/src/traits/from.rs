//! Conversion traits for ink! entities.

use ra_ap_syntax::{AstNode, SyntaxNode};

use crate::InkAttribute;

/// Implemented by ink! entities that wrap a syntax node.
pub trait FromSyntax {
    /// Returns the syntax node for the ink! entity.
    fn syntax(&self) -> &SyntaxNode;
}

/// Implemented by ink! entities that wrap an AST node.
pub trait FromAST {
    /// Associated AST node type.
    type AST: AstNode;

    /// Returns the AST node for the ink! entity.
    fn ast(&self) -> &Self::AST;
}

/// Blanket implementation of FromSyntax for ASTNode wrappers.
impl<T: FromAST> FromSyntax for T {
    fn syntax(&self) -> &SyntaxNode {
        self.ast().syntax()
    }
}

/// Implemented by ink! entities derived from an ink! attribute item.
pub trait FromInkAttribute {
    /// Returns true if the ink! entity can be derived for the ink! attribute.
    fn can_cast(attr: &InkAttribute) -> bool;

    /// Returns an ink! entity if one can be derived for the ink! attribute.
    fn cast(attr: InkAttribute) -> Option<Self>
    where
        Self: Sized;

    /// Returns the ink! attribute the ink! entity was derived from.
    fn ink_attr(&self) -> &InkAttribute;
}
