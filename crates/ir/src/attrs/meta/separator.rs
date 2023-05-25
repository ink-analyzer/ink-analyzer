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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (code, can_cast) in [
            // Can cast `=` symbol.
            (quote_as_str! { = }, true),
            // Can cast other symbols.
            (quote_as_str! { + }, false),
            (quote_as_str! { : }, false),
            (quote_as_str! { , }, false),
            (quote_as_str! { ? }, false),
        ] {
            // Check expected cast result.
            assert_eq!(
                MetaSeparator::cast(parse_first_syntax_token(code)).is_some(),
                can_cast,
                "meta name: {}",
                code
            );
        }
    }
}
