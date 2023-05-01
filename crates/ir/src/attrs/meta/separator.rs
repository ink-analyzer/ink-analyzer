//! ink! attribute meta item separator.

use ra_ap_syntax::{AstToken, SyntaxKind, SyntaxToken};

/// An ink! attribute meta item separator (i.e `=` symbol).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaSeparator {
    syntax: SyntaxToken,
}

impl AstToken for MetaSeparator {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::EQ
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        Self::can_cast(syntax.kind()).then_some(Self { syntax })
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
