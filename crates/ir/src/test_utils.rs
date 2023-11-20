//! Test utilities for IR types and abstractions.

#![cfg(test)]

use crate::InkAttribute;
use ra_ap_syntax::{ast, NodeOrToken, SyntaxKind, SyntaxNode};
use ra_ap_syntax::{AstNode, SourceFile, SyntaxElement, SyntaxToken};

/// Returns the first syntax token in the code snippet.
pub fn parse_first_syntax_token(code: &str) -> SyntaxToken {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants_with_tokens()
        .find_map(NodeOrToken::into_token)
        .unwrap()
}

/// Returns the first syntax node in the code snippet.
pub fn parse_first_syntax_node(code: &str) -> SyntaxNode {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find(|node| node.kind() != SyntaxKind::SOURCE_FILE)
        .unwrap()
}

/// Returns the first AST node of the generic type in the code snippet.
pub fn parse_first_ast_node_of_type<T>(code: &str) -> T
where
    T: AstNode,
{
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find_map(T::cast)
        .unwrap()
}

/// Returns the first attribute in the code snippet.
pub fn parse_first_attribute(code: &str) -> ast::Attr {
    parse_first_ast_node_of_type(code)
}

/// Returns the first ink! attribute in the code snippet.
pub fn parse_first_ink_attribute(code: &str) -> InkAttribute {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .descendants()
        .find_map(|node| InkAttribute::cast(ast::Attr::cast(node)?))
        .unwrap()
}

/// Returns all syntax elements for the code snippet.
pub fn parse_syntax_elements(code: &str) -> Vec<SyntaxElement> {
    SourceFile::parse(code)
        .tree()
        .syntax()
        .children_with_tokens()
        .collect()
}
