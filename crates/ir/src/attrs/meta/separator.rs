//! ink! attribute meta item separator.

use ra_ap_syntax::SyntaxKind::EQ;
use ra_ap_syntax::{AstToken, SyntaxKind, SyntaxToken};

/// An ink! attribute meta item separator (i.e `=` symbol).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaSeparator {
    syntax: SyntaxToken,
}

impl AstToken for MetaSeparator {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EQ
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
