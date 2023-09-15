//! AST trait extensions.

use ra_ap_syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, TextRange};

/// Convenience abstraction for shared methods of syntax tree types (i.e. SyntaxToken, SyntaxNode and SyntaxElement).
pub trait IsSyntax {
    fn kind(&self) -> SyntaxKind;

    fn text_range(&self) -> TextRange;

    fn parent(&self) -> Option<SyntaxNode>;
}

/// Implements [`IsSyntax`] for the syntax tree type (e.g. SyntaxToken, SyntaxNode and SyntaxElement).
macro_rules! impl_is_syntax {
    ($syntax_type:ty) => {
        impl IsSyntax for $syntax_type {
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
    };
}

impl_is_syntax!(SyntaxToken);
impl_is_syntax!(SyntaxNode);
impl_is_syntax!(SyntaxElement);
