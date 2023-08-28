//! AST traversal utilities.

use ra_ap_syntax::ast::HasAttrs;
use ra_ap_syntax::{ast, AstNode, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::traits::HasParent;

/// Returns the closest AST ancestor of a specific type for the syntax element.
pub fn closest_ancestor_ast_type<I, T>(item: &I) -> Option<T>
where
    I: HasParent,
    T: AstNode,
{
    let parent = item.parent_node()?;
    if T::can_cast(parent.kind()) {
        T::cast(parent)
    } else {
        closest_ancestor_ast_type(&parent)
    }
}

/// Returns parent [AST Item](https://github.com/rust-lang/rust-analyzer/blob/master/crates/syntax/src/ast/generated/nodes.rs#L1589-L1610)
/// for the syntax node.
pub fn parent_ast_item(node: &SyntaxNode) -> Option<ast::Item> {
    closest_ancestor_ast_type::<SyntaxNode, ast::Item>(node).and_then(|item| {
        if node.kind() == SyntaxKind::ATTR {
            // If the subject is an attribute, we make sure it's actually applied to the AST item.
            // This handles the case where an attribute is not really applied to any AST item.
            item.attrs()
                .any(|attr| attr.syntax() == node)
                .then_some(item)
        } else {
            Some(item)
        }
    })
}

/// Returns the first syntax token for the syntax node.
pub fn first_child_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.first_child_or_token().and_then(|child| match child {
        SyntaxElement::Token(token) => Some(token),
        SyntaxElement::Node(node) => first_child_token(&node),
    })
}

/// Returns the last syntax token for the syntax node.
pub fn last_child_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.last_child_or_token().and_then(|child| match child {
        SyntaxElement::Token(token) => Some(token),
        SyntaxElement::Node(node) => last_child_token(&node),
    })
}

/// Returns the closest non-trivia token based on the step expression.
pub fn closest_non_trivia_token<F>(token: &SyntaxToken, step_expr: F) -> Option<SyntaxToken>
where
    F: Fn(&SyntaxToken) -> Option<SyntaxToken>,
{
    closest_item_which(
        token,
        step_expr,
        |subject| !subject.kind().is_trivia(),
        |subject| !subject.kind().is_trivia(),
    )
}

/// Returns the closest non-trivia token based on the input predicates.
pub fn closest_item_which<T, S, G, H>(
    token: &T,
    step_expr: S,
    goal_expr: G,
    halt_expr: H,
) -> Option<T>
where
    S: Fn(&T) -> Option<T>,
    G: Fn(&T) -> bool,
    H: Fn(&T) -> bool,
{
    (step_expr)(token).and_then(|subject| {
        if goal_expr(&subject) {
            Some(subject)
        } else if halt_expr(&subject) {
            None
        } else {
            closest_item_which(&subject, step_expr, goal_expr, halt_expr)
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use quote::quote;
    use test_utils::quote_as_str;

    #[test]
    fn parent_ast_item_works() {
        let code = quote! {
            #[ink::contract]
            mod my_contract {
                #[ink(event)]
                pub struct MyEvent {
                    #[ink(topic)]
                    field_1: i32,
                    field_2: bool,
                }
            }
        };

        let module = parse_first_ast_node_of_type::<ast::Module>(quote_as_str! { #code });
        let struct_item = parse_first_ast_node_of_type::<ast::Struct>(quote_as_str! { #code });
        let field = parse_first_ast_node_of_type::<ast::RecordField>(quote_as_str! { #code });

        // struct is the AST parent of the field.
        assert_eq!(
            parent_ast_item(field.syntax())
                .unwrap()
                .syntax()
                .text_range(),
            struct_item.syntax().text_range()
        );

        // module is the AST parent of the struct.
        assert_eq!(
            parent_ast_item(struct_item.syntax())
                .unwrap()
                .syntax()
                .text_range(),
            module.syntax().text_range()
        );
    }
}
