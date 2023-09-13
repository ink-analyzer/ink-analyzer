//! AST trait extensions.

use ra_ap_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};

/// Convenience abstraction for shared methods of syntax tree types (i.e SyntaxToken, SyntaxNode and SyntaxElement).
pub trait IsSyntax {
    fn kind(&self) -> SyntaxKind;

    fn text_range(&self) -> TextRange;
    fn parent(&self) -> Option<SyntaxNode>;
}

impl IsSyntax for SyntaxToken {
    fn kind(&self) -> SyntaxKind {
        self.kind()
    }

    fn text_range(&self) -> TextRange {
        self.text_range()
    }

    fn parent(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}

impl IsSyntax for SyntaxNode {
    fn kind(&self) -> SyntaxKind {
        self.kind()
    }

    fn text_range(&self) -> TextRange {
        self.text_range()
    }

    fn parent(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}

impl IsSyntax for SyntaxElement {
    fn kind(&self) -> SyntaxKind {
        self.kind()
    }

    fn text_range(&self) -> TextRange {
        self.text_range()
    }

    fn parent(&self) -> Option<SyntaxNode> {
        self.parent()
    }
}
