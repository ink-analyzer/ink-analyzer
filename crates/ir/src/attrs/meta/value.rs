//! ink! attribute meta item value.

use std::fmt;

use impl_serde::serialize as serde_hex;
use itertools::Itertools;
use ra_ap_syntax::{
    ast, AstNode, AstToken, Edition, SourceFile, SyntaxElement, SyntaxKind, SyntaxToken, TextRange,
    TextSize,
};

use crate::IsIntId;

/// An ink! attribute meta item value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaValue {
    /// The syntax elements from which the meta value is derived.
    elements: Vec<SyntaxElement>,
    /// A syntax token that's equivalent to the meta value with trivia ignore (i.e. no whitespace and comments).
    /// NOTE: Only used for `_` and `@` symbols which are used as literal symbols in selectors.
    token: Option<SyntaxToken>,
    /// An expression that's equivalent to the meta value but with wrong text ranges and offsets.
    expr: Option<ast::Expr>,
}

impl MetaValue {
    /// Parse (if possible) a sequence of `SyntaxElement`s into an `Expr` or a `SyntaxToken` that represents the meta value.
    ///
    /// Invariant: Either `Self::token` or `Self::expr` must be `Some` for a `MetaValue` to be created.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L40-L49>.
    ///
    /// Ref: <https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax>.
    pub fn parse(elems: &[SyntaxElement]) -> Option<Self> {
        if !elems.is_empty() {
            // Try to parse the `_` and `@` tokens.
            if let Some((non_trivia_elem,)) = elems
                .iter()
                .filter(|elem| !elem.kind().is_trivia())
                .collect_tuple()
            {
                if matches!(
                    non_trivia_elem.kind(),
                    SyntaxKind::UNDERSCORE | SyntaxKind::AT
                ) {
                    if let Some(token) = non_trivia_elem.to_owned().into_token() {
                        return Some(Self {
                            elements: elems.to_owned(),
                            token: Some(token),
                            expr: None,
                        });
                    }
                }
            }

            // Try to parse as an expression.
            // For ink!, we're only interested in:
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L51-L56>.
            // 1. Literals (mostly integers, strings and true/false, e.g 1, 0xDEADBEEF, "my_namespace", true).
            // Ref: <https://doc.rust-lang.org/reference/expressions/literal-expr.html>
            // 2. Paths e.g my::env::Types.
            // Ref: <https://doc.rust-lang.org/reference/expressions/path-expr.html>.
            // NOTE: `_` is already handled as a token above, so underscore expressions are ignored.
            // Ref: <https://doc.rust-lang.org/reference/expressions/underscore-expr.html>.
            let arg_text = elems.iter().map(ToString::to_string).join("");
            // TODO: Does the edition matter for the literal and path expressions we care about?
            if let Some(expr) =
                ra_ap_syntax::hacks::parse_expr_from_str(&arg_text, Edition::Edition2021)
            {
                if matches!(expr, ast::Expr::Literal(_) | ast::Expr::PathExpr(_)) {
                    return Some(Self {
                        elements: elems.to_owned(),
                        token: None,
                        expr: Some(expr),
                    });
                }
            }
        }

        None
    }

    /// Returns the syntax elements.
    pub fn elements(&self) -> &[SyntaxElement] {
        &self.elements
    }

    /// Returns the syntax kind of meta value.
    pub fn kind(&self) -> SyntaxKind {
        self.token
            .as_ref()
            .map(SyntaxToken::kind)
            .or_else(|| {
                self.expr.as_ref().map(|expr| match expr {
                    ast::Expr::Literal(lit) => lit.token().kind(),
                    ast::Expr::PathExpr(_) => SyntaxKind::PATH,
                    _ => expr.syntax().kind(),
                })
            })
            .expect("Either `MetaValue::token` or `MetaValue::expr` should be `Some`")
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
                    .expect("Elements should never be empty")
                    .text_range()
                    .start(),
            ),
            end.unwrap_or(
                self.elements
                    .last()
                    .expect("Elements should never be empty")
                    .text_range()
                    .start(),
            ),
        )
    }

    /// Returns true if the value is a wildcard/underscore symbol.
    pub fn is_wildcard(&self) -> bool {
        self.kind() == SyntaxKind::UNDERSCORE
    }

    /// Returns true if the value is an `@` symbol.
    ///
    /// NOTE: We intentionally don't use the "wildcard complement" language here because that needs
    /// to be able to recognize the well-known value as well.
    /// See [`SelectorArg::is_wildcard_complement`][crate::SelectorArg::is_wildcard_complement] for details.
    pub fn is_at_symbol(&self) -> bool {
        self.kind() == SyntaxKind::AT
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a `u16`.
    pub fn as_u16(&self) -> Option<u16> {
        self.as_int()
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a `u32`.
    pub fn as_u32(&self) -> Option<u32> {
        self.as_int()
    }

    /// Converts the value if it's an integer literal (decimal or hexadecimal) into a integer.
    pub fn as_int<T>(&self) -> Option<T>
    where
        T: IsIntId,
    {
        if self.kind() == SyntaxKind::INT_NUMBER {
            let value = self.to_string();
            if value.starts_with("0x") {
                // Parse as hex.
                T::from_str_radix(value.strip_prefix("0x").unwrap(), 16).ok()
            } else {
                // Parse as decimal.
                value.parse::<T>().ok()
            }
        } else {
            None
        }
    }

    /// Converts the value if it's a boolean literal (true or false keyword) into a `bool`.
    pub fn as_bool(&self) -> Option<bool> {
        match self.kind() {
            SyntaxKind::TRUE_KW => Some(true),
            SyntaxKind::FALSE_KW => Some(false),
            _ => None,
        }
    }

    /// Converts the value if it's a string literal into a `String`.
    pub fn as_string(&self) -> Option<String> {
        (self.kind() == SyntaxKind::STRING).then_some(
            // Parses string value (removes escaped leading and trailing quotes).
            self.to_string()
                .chars()
                .skip_while(|char| *char == '\"')
                .take_while(|char| *char != '\"')
                .join(""),
        )
    }

    /// Converts the value if it's a path expression into a `Path` with an inaccurate text range.
    pub fn as_path_with_inaccurate_text_range(&self) -> Option<ast::Path> {
        self.expr.as_ref().and_then(|expr| match expr {
            ast::Expr::PathExpr(path) => path.path(),
            _ => None,
        })
    }

    /// Converts the string value (if any) to a Rust identifier (`Ident`) (if possible).
    pub fn as_ident(&self) -> Option<ast::Ident> {
        self.as_string().and_then(|value| {
            // Parse sanitized value and find the first identifier.
            // TODO: Does the edition matter for identifiers?
            let file = SourceFile::parse(&value, Edition::Edition2021).tree();

            // Retrieve the first `ident` in the syntax tree.
            let ident = file
                .syntax()
                .descendants_with_tokens()
                .find_map(|elem| ast::Ident::cast(elem.into_token()?))?;

            // Parsed identifier must be equal to the sanitized meta value.
            (ident.text() == value).then_some(ident)
        })
    }

    /// Converts the string value (if any) to a hex Vec (if possible).
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v5.0.0-rc.1/crates/ink/ir/src/ir/event/signature_topic.rs#L43-L69>
    pub fn as_hex_vec(&self) -> Option<Vec<u8>> {
        self.as_string()
            .and_then(|value| serde_hex::from_hex(&value).ok())
    }

    /// Converts the string value (if any) to a 32 byte hex array (if possible).
    pub fn as_hex_32(&self) -> Option<[u8; 32]> {
        self.as_hex_vec().and_then(|value| value.try_into().ok())
    }

    /// Compares equality with another meta value while ignoring trivia (i.e. whitespace and comments).
    pub fn trivia_insensitive_eq(&self, other: &MetaValue) -> bool {
        let strip_trivia = |elems: &[SyntaxElement]| {
            elems
                .iter()
                .filter(|node| !node.kind().is_trivia())
                .join("")
        };
        strip_trivia(&self.elements) == strip_trivia(other.elements())
    }
}

impl fmt::Display for MetaValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(token) = &self.token {
            token.fmt(f)
        } else {
            self.expr
                .as_ref()
                .expect("Either `MetaValue::token` or `MetaValue::expr` should be `Some`")
                .fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_source;
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
            // Can cast `_` symbol.
            (quote_as_str! { _ }, true),
            // Can cast `@` symbol.
            (quote_as_str! { @ }, true),
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
            let elements: Vec<_> = parse_source(code)
                .syntax()
                .children_with_tokens()
                .map(|elem| {
                    // Un-peel error nodes and return their token children (if any)
                    // Needed for `_` and `@`.
                    if elem.kind() == SyntaxKind::ERROR {
                        if let Some(error) = elem.as_node() {
                            if let Some(token) = error.first_token() {
                                return SyntaxElement::from(token);
                            }
                        }
                    }
                    elem
                })
                .collect();
            // Check expected cast result.
            assert_eq!(
                MetaValue::parse(&elements).is_some(),
                can_cast,
                "meta value: {code}"
            );
        }
    }
}
