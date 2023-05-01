//! ink! attribute meta item name.

use ra_ap_syntax::{AstToken, SyntaxKind, SyntaxToken};
use std::fmt;

/// An ink! attribute meta item name.
// We need a custom type because can't simply use `Ident`
// ink! attributes include an `impl` argument which is parsed as the `impl` keyword.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaName {
    syntax: SyntaxToken,
}

impl AstToken for MetaName {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::IDENT | SyntaxKind::IMPL_KW)
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        Self::can_cast(syntax.kind()).then_some(Self { syntax })
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}

impl fmt::Display for MetaName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.syntax.fmt(f)
    }
}
