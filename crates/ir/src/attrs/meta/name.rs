//! ink! attribute meta item name.

use ra_ap_syntax::{AstToken, SyntaxKind, SyntaxToken};

/// An ink! attribute meta item name.
// We need a custom type because can't simply use `Ident`
// ink! attributes includes an `impl` argument which is parsed as the `impl` keyword.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaName {
    syntax: SyntaxToken,
}

impl AstToken for MetaName {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::IDENT | SyntaxKind::IMPL_KW)
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

impl ToString for MetaName {
    fn to_string(&self) -> String {
        self.syntax.to_string()
    }
}
