//! ink! attribute argument meta item value.

use itertools::Itertools;
use ra_ap_syntax::ast::Expr;
use ra_ap_syntax::{AstNode, SyntaxElement, SyntaxKind, TextRange, TextSize};

/// An ink! attribute argument value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaValue {
    expr: Expr,
    elements: Vec<SyntaxElement>,
}

impl MetaValue {
    // See https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax for grammar
    pub fn parse(elems: Vec<SyntaxElement>) -> Option<Self> {
        let arg_text = elems
            .clone()
            .into_iter()
            .map(|elem| elem.to_string())
            .join("");

        // Try to parse as an expression
        // For ink!, we're specifically interested in:
        // 1. Literals e.g 1, 0xDEADBEEF, "my_namespace", true
        // https://doc.rust-lang.org/reference/expressions/literal-expr.html
        // 2. Paths e.g my::env::Types (i.e paths)
        // https://doc.rust-lang.org/reference/expressions/path-expr.html
        // 3. Underscore expressions/Wildcard i.e _ (e.g for wildcard selectors)
        // https://doc.rust-lang.org/reference/expressions/underscore-expr.html
        let expr = ra_ap_syntax::hacks::parse_expr_from_str(&arg_text);
        expr.map(|exp| Self {
            expr: exp,
            elements: elems,
        })
    }

    pub fn elements(&self) -> &Vec<SyntaxElement> {
        &self.elements
    }

    pub fn as_expr_with_wrong_text_range(&self) -> &Expr {
        &self.expr
    }

    pub fn kind(&self) -> SyntaxKind {
        self.as_expr_with_wrong_text_range().syntax().kind()
    }

    pub fn text_range(&self) -> Option<TextRange> {
        let mut first: Option<TextSize> = None;
        let mut last: Option<TextSize> = None;

        for token in &self.elements {
            if !token.kind().is_trivia() {
                if first.is_none() {
                    first = Some(token.text_range().start());
                }

                last = Some(token.text_range().end());
            }
        }

        if let Some(start) = first {
            if let Some(end) = last {
                return Some(TextRange::new(start, end));
            }
        }
        None
    }
}
