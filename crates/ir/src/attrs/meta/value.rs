//! ink! attribute meta item value.

use itertools::Itertools;
use ra_ap_syntax::ast::Expr;
use ra_ap_syntax::{AstNode, SyntaxElement, SyntaxKind, TextRange, TextSize};

/// An ink! attribute meta item value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetaValue {
    expr: Expr,
    elements: Vec<SyntaxElement>,
}

impl MetaValue {
    /// Parse (if possible) a sequence of `SyntaxElement`s into an `Expr` that represents meta value.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ast/attr_args.rs#L40-L49>.
    ///
    /// Ref: <https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax>.
    pub fn parse(elems: Vec<SyntaxElement>) -> Option<Self> {
        if !elems.is_empty() {
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
            return expr.map(|exp| Self {
                expr: exp,
                elements: elems,
            });
        }
        None
    }

    /// Returns the syntax elements.
    pub fn elements(&self) -> &Vec<SyntaxElement> {
        &self.elements
    }

    /// Returns the equivalent expression with an inaccurate text range.
    pub fn as_expr_with_inaccurate_text_range(&self) -> &Expr {
        &self.expr
    }

    /// Returns the syntax kind of meta value.
    pub fn kind(&self) -> SyntaxKind {
        match &self.expr {
            Expr::Literal(lit) => lit.token().kind(),
            Expr::PathExpr(path_expr) => {
                if let Some(path) = path_expr.path() {
                    path.syntax().kind()
                } else {
                    path_expr.syntax().kind()
                }
            }
            Expr::UnderscoreExpr(underscore_expr) => {
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
}

impl ToString for MetaValue {
    fn to_string(&self) -> String {
        self.expr.to_string()
    }
}
