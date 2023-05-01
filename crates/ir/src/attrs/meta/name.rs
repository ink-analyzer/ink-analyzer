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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quote_as_str;
    use crate::test_utils::*;

    #[test]
    fn cast_works() {
        for (code, can_cast) in [
            // Can cast identifier.
            (quote_as_str! { my_ident }, true),
            (quote_as_str! { another_random_ident }, true),
            // Can cast `impl` keyword.
            (quote_as_str! { impl }, true),
            // Can't cast other keywords.
            (quote_as_str! { fn }, false),
            (quote_as_str! { struct }, false),
            (quote_as_str! { enum }, false),
            (quote_as_str! { const }, false),
        ] {
            // Check expected cast result.
            assert_eq!(
                MetaName::cast(parse_first_syntax_token(code)).is_some(),
                can_cast,
                "meta name: {}",
                code
            );
        }
    }
}
