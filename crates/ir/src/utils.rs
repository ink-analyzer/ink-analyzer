//! ink! IR utilities.

use itertools::{Either, Itertools};
use ra_ap_syntax::ast::{Attr, Item};
use ra_ap_syntax::{AstNode, SyntaxKind, SyntaxNode};

use crate::InkAttribute;

/// Casts a syntax nodes to an ink! attribute or returns None.
fn ink_attribute_from_node(node: SyntaxNode) -> Option<InkAttribute> {
    Attr::cast(node).and_then(InkAttribute::cast)
}

/// Returns ink! attributes for the syntax node.
pub fn ink_attrs(node: &SyntaxNode) -> Vec<InkAttribute> {
    // ink! attributes are children of the current syntax node.
    node.children()
        .filter_map(ink_attribute_from_node)
        .collect()
}

/// Returns ink! attributes for all the syntax node's descendants.
pub fn ink_attrs_descendants(node: &SyntaxNode) -> Vec<InkAttribute> {
    // Simply calling descendants on the syntax node directly would include the syntax node's own ink! attributes.
    // So we get the non-attribute children first and then call descendants on all of them.
    node.children()
        .filter(|child| child.kind() != SyntaxKind::ATTR)
        .flat_map(|child| {
            child
                .descendants()
                .filter_map(ink_attribute_from_node)
                .collect::<Vec<InkAttribute>>()
        })
        .collect()
}

/// Returns ink! attributes for all the syntax node's descendants
/// that don't have any ink! ancestors between them and the current node.
pub fn ink_attrs_closest_descendants(node: &SyntaxNode) -> Vec<InkAttribute> {
    // Simply calling children on the syntax node directly would include the syntax node's own ink! attributes.
    // So we get the non-attribute children first and then either get their ink! attributes or return them if they have none.
    let (ink_children_groups, non_ink_children): (Vec<Vec<InkAttribute>>, Vec<SyntaxNode>) = node
        .children()
        .filter(|child| child.kind() != SyntaxKind::ATTR)
        .partition_map(|child| {
            let child_ink_attrs = ink_attrs(&child);
            if !child_ink_attrs.is_empty() {
                return Either::Left(child_ink_attrs);
            }
            Either::Right(child)
        });

    // Flatten collected ink attributes.
    let mut attrs: Vec<InkAttribute> = ink_children_groups.into_iter().flatten().collect();

    // Recurse on children with no ink! attributes.
    if !non_ink_children.is_empty() {
        attrs.append(
            &mut non_ink_children
                .iter()
                .flat_map(ink_attrs_closest_descendants)
                .collect(),
        );
    }

    attrs
}

/// Returns ink! attributes in the syntax node's scope.
/// This includes both the nodes own ink! attributes and those of all of it's descendants.
pub fn ink_attrs_in_scope(node: &SyntaxNode) -> Vec<InkAttribute> {
    // Get node's ink! attributes.
    let mut attrs = ink_attrs(node);
    // Append ink! attributes of all descendants.
    attrs.append(&mut ink_attrs_descendants(node));

    attrs
}

/// Returns ink! attributes for all the syntax node's ancestors.
pub fn ink_attrs_ancestors(node: &SyntaxNode) -> Vec<InkAttribute> {
    // Calling ancestors directly would include the current node.
    // (it's a rowan/ra_ap_syntax quirk https://github.com/rust-analyzer/rowan/blob/v0.15.11/src/cursor.rs#L625).
    // So we get the parent first and then call ancestors on that.
    let mut attrs = Vec::new();
    if let Some(parent) = node.parent() {
        attrs = parent
            .ancestors()
            .flat_map(|ancestor| ink_attrs(&ancestor))
            .collect();
    }
    attrs
}

/// Returns ink! attributes for all the syntax node's ancestors
/// that don't have any ink! ancestors between them and the current node.
pub fn ink_attrs_closest_ancestors(node: &SyntaxNode) -> Vec<InkAttribute> {
    let mut attrs = Vec::new();
    if let Some(parent) = node.parent() {
        attrs = ink_attrs(&parent);
        if attrs.is_empty() {
            // Only recurse if parent node has no ink! attributes.
            attrs = ink_attrs_closest_ancestors(&parent);
        }
    }
    attrs
}

/// Returns parent [AST Item](https://github.com/rust-lang/rust-analyzer/blob/master/crates/syntax/src/ast/generated/nodes.rs#L1589-L1610)
/// for the syntax node.
pub fn parent_ast_item(node: &SyntaxNode) -> Option<Item> {
    let parent = node.parent()?;
    if let Some(item) = Item::cast(parent.clone()) {
        Some(item)
    } else {
        parent_ast_item(&parent)
    }
}

/// Quasi-quotation macro that accepts input like the `quote!` macro
/// and returns string slice (`&str`) instead of a `TokenStream`.
#[macro_export]
macro_rules! quote_as_str {
    ($($tt:tt)*) => {
        quote::quote!($($tt)*).to_string().as_str()
    };
}
