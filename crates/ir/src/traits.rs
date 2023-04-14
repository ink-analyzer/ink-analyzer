//! ink! IR traits.

use ra_ap_syntax::{AstNode, SyntaxKind, SyntaxNode};

use crate::{utils, InkAttribute};

/// Implemented by IR items that wrap a syntax node.
pub trait FromSyntax {
    /// Returns the syntax node for the IR item.
    fn syntax(&self) -> &SyntaxNode;
}

/// Implemented by IR items that wrap an AST node.
pub trait FromAST: FromSyntax {
    /// Associated AST node type.
    type AST: AstNode;

    /// Returns the AST node for the IR item.
    fn ast(&self) -> &Self::AST;
}

/// Blanket implementation of FromSyntax for ASTNode wrappers.
impl<T: FromAST> FromSyntax for T {
    fn syntax(&self) -> &SyntaxNode {
        self.ast().syntax()
    }
}

/// Implemented by IR items that wrap an ink! attribute item.
pub trait FromInkAttribute: FromSyntax {
    /// Returns the ink! attribute item for the IR item.
    fn ink_attr(&self) -> &InkAttribute;
}

/// Convenience methods implemented by IR items.
pub trait IRItem {
    /// Returns the syntax kind for the IR item.
    fn syntax_kind(&self) -> SyntaxKind;

    /// Returns the syntax tree parent for the IR item.
    fn syntax_parent(&self) -> Option<SyntaxNode>;

    /// Returns the kind of the syntax tree parent for the IR item.
    fn syntax_parent_kind(&self) -> Option<SyntaxKind>;

    /// Returns ink! attributes for the IR item.
    fn ink_attrs(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's descendants.
    fn ink_attrs_descendants(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's descendants
    /// that don't have any ink! ancestors between them and the item.
    fn ink_attrs_closest_descendants(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's ancestors.
    fn ink_ancestors(&self) -> Vec<InkAttribute>;

    /// Returns ink! attributes for all the IR item's ancestors
    /// that don't have any ink! ancestors between them and the item.
    fn ink_closest_ancestors(&self) -> Vec<InkAttribute>;
}

/// Blanket implementation of IRItem for syntax node wrappers.
impl<T> IRItem for T
where
    T: FromSyntax,
{
    fn syntax_kind(&self) -> SyntaxKind {
        self.syntax().kind()
    }

    fn syntax_parent(&self) -> Option<SyntaxNode> {
        self.syntax().parent()
    }

    fn syntax_parent_kind(&self) -> Option<SyntaxKind> {
        Some(self.syntax_parent()?.kind())
    }

    fn ink_attrs(&self) -> Vec<InkAttribute> {
        utils::ink_attrs(self.syntax())
    }

    fn ink_attrs_descendants(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_descendants(self.syntax())
    }

    fn ink_attrs_closest_descendants(&self) -> Vec<InkAttribute> {
        utils::ink_attrs_closest_descendants(self.syntax())
    }

    fn ink_ancestors(&self) -> Vec<InkAttribute> {
        utils::ink_ancestors(self.syntax())
    }

    fn ink_closest_ancestors(&self) -> Vec<InkAttribute> {
        utils::ink_closest_ancestors(self.syntax())
    }
}
