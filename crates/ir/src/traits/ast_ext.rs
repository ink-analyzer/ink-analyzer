//! AST trait extensions.

use ra_ap_syntax::{SyntaxElement, SyntaxNode, SyntaxToken};

pub trait HasParent {
    fn parent_node(&self) -> Option<SyntaxNode>;
}

impl HasParent for SyntaxToken {
    fn parent_node(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}

impl HasParent for SyntaxNode {
    fn parent_node(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}

impl HasParent for SyntaxElement {
    fn parent_node(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}
