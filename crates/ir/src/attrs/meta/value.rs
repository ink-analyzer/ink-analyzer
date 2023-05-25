//! ink! attribute meta item value.

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, SyntaxElement, SyntaxKind, TextRange, TextSize};
use std::fmt;

/// An ink! attribute meta item value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaValue {
    /// An expression that's equivalent to the meta value but with wrong text ranges and offsets.
    expr: ast::Expr,
    /// The syntax elements from which the meta value is derived.
    elements: Vec<SyntaxElement>,
}

impl MetaValue {
    /// Parse (if possible) a sequence of `SyntaxElement`s into an `Expr` that represents the meta value.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L40-L49>.
    ///
    /// Ref: <https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax>.
    pub fn parse(elems: &[SyntaxElement]) -> Option<Self> {
        (!elems.is_empty()).then(|| {
            let arg_text = elems.iter().map(|elem| elem.to_string()).join("");

            // Try to parse as an expression.
            // For ink!, we're only interested in:
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L51-L56>.
            // Ref: <https://doc.rust-lang.org/reference/expressions/literal-expr.html>
            // 1. Literals (mostly integers, strings and true/false, e.g 1, 0xDEADBEEF, "my_namespace", true).
            // Ref: <https://doc.rust-lang.org/reference/expressions/path-expr.html>.
            // 2. Paths e.g my::env::Types.
            // Ref: <https://doc.rust-lang.org/reference/expressions/underscore-expr.html>.
            // 3. Underscore expressions/Wildcard i.e _ (e.g for wildcard selectors).
            ra_ap_syntax::hacks::parse_expr_from_str(&arg_text).and_then(|expr| {
                (matches!(
                    expr,
                    ast::Expr::Literal(_) | ast::Expr::PathExpr(_) | ast::Expr::UnderscoreExpr(_)
                ))
                .then_some(Self {
                    expr,
                    elements: elems.to_owned(),
                })
            })
        })?
    }

    /// Returns the syntax elements.
    pub fn elements(&self) -> &[SyntaxElement] {
        &self.elements
    }

    /// Returns the equivalent expression with an inaccurate text range.
    pub fn as_expr_with_inaccurate_text_range(&self) -> &ast::Expr {
        &self.expr
    }

    /// Returns the syntax kind of meta value.
    pub fn kind(&self) -> SyntaxKind {
        match &self.expr {
            ast::Expr::Literal(lit) => lit.token().kind(),
            ast::Expr::PathExpr(path_expr) => {
                if let Some(path) = path_expr.path() {
                    path.syntax().kind()
                } else {
                    path_expr.syntax().kind()
                }
            }
            ast::Expr::UnderscoreExpr(underscore_expr) => {
                if let Some(underscore) = underscore_expr.underscore_token() {
                    underscore.kind()
                } else {
                    underscore_expr.syntax().kind()
                }
            }
            _ => self.expr.syntax().kind(),
        }
    }

    /// Returns the accurate text range for the parsed meta value.
    pub fn text_range(&self) -> TextRange {
        let mut start: Option<TextSize> = None;
        let mut end: Option<TextSize> = None;

        // Try to not include leading and trailing trivia in the range.
        for token in &self.elements {
            if !token.kind().is_trivia() {
                if start.is_none() {
                    start = Some(token.text_range().start());
                }
                end = Some(token.text_range().end());
            }
        }

        TextRange::new(
            start.unwrap_or(
                self.elements
                    .first()
                    .expect("parse method ensures that elements is never be empty")
                    .text_range()
                    .start(),
            ),
            end.unwrap_or(
                self.elements
                    .last()
                    .expect("parse method ensures that elements is never be empty")
                    .text_range()
                    .start(),
            ),
        )
    }

    /// Returns true if the value is a wildcard/underscore.
    pub fn is_wildcard(&self) -> bool {
        matches!(
            self.kind(),
            SyntaxKind::UNDERSCORE | SyntaxKind::UNDERSCORE_EXPR
        )
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a `u32`.
    pub fn as_u32(&self) -> Option<u32> {
        (self.kind() == SyntaxKind::INT_NUMBER).then(|| {
            let value = self.to_string();
            if value.starts_with("0x") {
                // Check as hex.
                u32::from_str_radix(value.strip_prefix("0x").unwrap(), 16).ok()
            } else {
                // Check as decimal.
                value.parse::<u32>().ok()
            }
        })?
    }

    /// Converts the value if it's a boolean literal (true or false keyword) into a `bool`.
    pub fn as_boolean(&self) -> Option<bool> {
        match self.kind() {
            SyntaxKind::TRUE_KW => Some(true),
            SyntaxKind::FALSE_KW => Some(false),
            _ => None,
        }
    }

    /// Converts the value if it's a string literal into a `String`.
    pub fn as_string(&self) -> Option<String> {
        (self.kind() == SyntaxKind::STRING).then(|| {
            let mut value = self.to_string();
            // Strip leading and trailing escaped quotes.
            if value.starts_with('\"') {
                value = value
                    .strip_prefix('\"')
                    .expect("Should be able to strip prefix")
                    .to_string();
            }
            if value.ends_with('\"') {
                value = value
                    .strip_suffix('\"')
                    .expect("Should be able to strip suffix")
                    .to_string();
            }
            value
        })
    }
}

impl fmt::Display for MetaValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.expr.fmt(f)
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
            // Can cast integers.
            (quote_as_str! { 10 }, true),
            (quote_as_str! { 0xA }, true),
            // Can cast strings.
            (quote_as_str! { "Hello" }, true),
            (quote_as_str! { "foo, bar" }, true),
            // Can cast booleans (true/false).
            (quote_as_str! { true }, true),
            (quote_as_str! { false }, true),
            // Can cast paths.
            (quote_as_str! { my::env::Path }, true),
            (quote_as_str! { ::my::env::Path }, true),
            (quote_as_str! { Path }, true),
            // Can cast wildcards/underscore expressions.
            (quote_as_str! { _ }, true),
            // Can't cast keywords.
            (quote_as_str! { impl }, false),
            (quote_as_str! { fn }, false),
            (quote_as_str! { struct }, false),
            (quote_as_str! { enum }, false),
            (quote_as_str! { const }, false),
            // Can't cast quasi-statements and/or invalid expressions.
            (quote_as_str! { x = 10 }, false),
            (quote_as_str! { x = true }, false),
            (quote_as_str! { x: bool }, false),
            (quote_as_str! { x, y, z }, false),
            (quote_as_str! { x y z }, false),
        ] {
            // Check expected cast result.
            assert_eq!(
                MetaValue::parse(&parse_syntax_elements(code)).is_some(),
                can_cast,
                "meta value: {}",
                code
            );
        }
    }
}
