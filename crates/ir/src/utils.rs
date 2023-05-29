//! ink! IR utilities.

use itertools::Itertools;
use ra_ap_syntax::ast::HasAttrs;
use ra_ap_syntax::{ast, AstNode, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::iter::IterSuccessors;
use crate::{
    Constructor, FromInkAttribute, FromSyntax, HasParent, InkArg, InkArgKind, InkAttrData,
    InkAttribute, InkAttributeKind, InkImpl, InkImplItem, InkMacroKind, Message,
};

/// Casts a syntax node to an ink! attribute (if possible).
fn ink_attribute_from_node(node: SyntaxNode) -> Option<InkAttribute> {
    ast::Attr::cast(node).and_then(InkAttribute::cast)
}

/// Returns ink! attributes for the syntax node.
pub fn ink_attrs(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> {
    // ink! attributes are children of the current syntax node.
    node.children().filter_map(ink_attribute_from_node)
}

/// Returns ink! attributes for all the syntax node's descendants.
pub fn ink_attrs_descendants(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> {
    // Simply calling descendants on the syntax node directly would include the syntax node's own ink! attributes.
    // So we get the non-attribute children first and then call descendants on all of them.
    node.children()
        .filter(|child| child.kind() != SyntaxKind::ATTR)
        .flat_map(|child| child.descendants().filter_map(ink_attribute_from_node))
}

/// Returns ink! attributes for all the syntax node's descendants
/// that don't have any ink! ancestor between them and the current node.
pub fn ink_attrs_closest_descendants(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> {
    // Simply calling children on the syntax node directly would include the syntax node's own ink! attributes.
    // So we get the non-attribute children first and then either get their ink! attributes or recurse them if they have none.
    node.children().flat_map(|child| {
        // Return child ink! attributes (if any), otherwise recurse on children with no ink! attributes..
        if ink_attrs(&child).next().is_some() {
            // Cast to a boxed iterator trait object, otherwise the opaque types will mismatch.
            Box::new(ink_attrs(&child)) as Box<dyn Iterator<Item = InkAttribute>>
        } else {
            // Otherwise recurse on children with no ink! attributes.
            Box::new(ink_attrs_closest_descendants(&child))
        }
    })
}

/// Returns ink! attributes in the syntax node's scope.
/// This includes both the nodes own ink! attributes and those of all of it's descendants.
pub fn ink_attrs_in_scope(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> {
    // Get node's ink! attributes.
    ink_attrs(node).chain(
        // Append ink! attributes of all descendants.
        ink_attrs_descendants(node),
    )
}

/// Returns ink! attributes for all the syntax node's ancestors.
pub fn ink_attrs_ancestors(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> + '_ {
    node.ancestors()
        .filter(move |ancestor| {
            // Ancestors includes the current node, so we filter it out first.
            // (it's a rowan/ra_ap_syntax quirk https://github.com/rust-analyzer/rowan/blob/v0.15.11/src/cursor.rs#L625).
            ancestor != node
                // Additionally, if the current node is an attribute,
                // we also filter out it's parent so that we get its ancestor attributes not its siblings.
                && (node.kind() != SyntaxKind::ATTR
                    || node.parent().map(|attr_parent| attr_parent.text_range())
                        != Some(ancestor.text_range()))
        })
        .flat_map(|ancestor| ink_attrs(&ancestor))
}

/// Returns ink! attributes for all the syntax node's ancestors
/// that don't have any ink! descendant between them and the current node.
pub fn ink_attrs_closest_ancestors(node: &SyntaxNode) -> impl Iterator<Item = InkAttribute> {
    IterSuccessors::new(
        if node.kind() == SyntaxKind::ATTR {
            // For attributes, we need to call parent on the parent node so that we get ancestor attributes not siblings.
            node.parent().and_then(|attr_parent| attr_parent.parent())
        } else {
            node.parent()
        },
        |source| {
            // Get a reference to the current node or return None to stop the recursion.
            source.as_ref().map(|current_node| {
                let has_attrs = ink_attrs(current_node).next().is_some();
                if has_attrs {
                    // Return the next iter of ink! attributes (if any).
                    (Some(ink_attrs(current_node)), None)
                } else {
                    // Otherwise the current node's parent becomes next source.
                    (None, current_node.parent())
                }
            })
        },
    )
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

/// Returns the closest AST ancestor is a specific type for the syntax element.
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

/// Returns the syntax node's descendant ink! entities of IR type `T`.
pub fn ink_descendants<T>(node: &SyntaxNode) -> impl Iterator<Item = T>
where
    T: FromInkAttribute,
{
    ink_attrs_descendants(node).filter_map(T::cast)
}

/// Returns the syntax node's descendant ink! entities of IR type `T` that don't have any
/// ink! ancestor between them and the current node.
pub fn ink_closest_descendants<T>(node: &SyntaxNode) -> impl Iterator<Item = T>
where
    T: FromInkAttribute,
{
    ink_attrs_closest_descendants(node).filter_map(T::cast)
}

/// Returns the syntax node's parent ink! entity of IR type `T` (if any).
pub fn ink_parent<T>(node: &SyntaxNode) -> Option<T>
where
    T: FromInkAttribute,
{
    parent_ast_item(node).and_then(|parent| ink_attrs(parent.syntax()).find_map(T::cast))
}

/// Returns the syntax node's ancestor ink! entities of IR type `T`.
pub fn ink_ancestors<'a, T>(node: &'a SyntaxNode) -> impl Iterator<Item = T> + 'a
where
    T: FromInkAttribute + 'a,
{
    ink_attrs_ancestors(node).filter_map(T::cast)
}

/// Returns the syntax node's ancestor ink! entities of IR type `T` that don't have any
/// ink! descendant between them and the current node.
pub fn ink_closest_ancestors<T>(node: &SyntaxNode) -> impl Iterator<Item = T>
where
    T: FromInkAttribute,
{
    ink_attrs_closest_ancestors(node).filter_map(T::cast)
}

/// Returns ink! arguments of the syntax node.
pub fn ink_args(node: &SyntaxNode) -> impl Iterator<Item = InkArg> {
    ink_attrs(node).flat_map(|attr| attr.args().to_owned())
}

/// Returns ink! arguments of a specific kind (if any) for the syntax node.
pub fn ink_args_by_kind(node: &SyntaxNode, kind: InkArgKind) -> impl Iterator<Item = InkArg> {
    ink_attrs(node)
        .flat_map(move |attr| attr.args().iter().cloned().find(|arg| *arg.kind() == kind))
}

/// Returns ink! argument of a specific kind (if any) for the syntax node.
pub fn ink_arg_by_kind(node: &SyntaxNode, kind: InkArgKind) -> Option<InkArg> {
    ink_attrs(node).find_map(|attr| attr.args().iter().cloned().find(|arg| *arg.kind() == kind))
}

/// Returns true if the ink! attribute can be a quasi-direct parent for an ink! callable entity
/// (i.e ink! constructor or ink! message).
fn is_possible_callable_ancestor(attr: &InkAttribute) -> bool {
    // ink! impl annotated closest descendants or an `impl` item annotated with ink! namespace.
    *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl)
        || (*attr.kind() == InkAttributeKind::Arg(InkArgKind::Namespace)
            && parent_ast_item(attr.syntax())
                .and_then(|item| ast::Impl::cast(item.syntax().to_owned()))
                .is_some())
}

/// Returns the syntax node's descendant ink! entities of IR type `T` that either don't have any
/// ink! ancestor or only have an ink! impl entity between them and the current node.
pub fn ink_callable_closest_descendants<T>(node: &SyntaxNode) -> impl Iterator<Item = T>
where
    T: FromSyntax + FromInkAttribute + InkImplItem,
{
    ink_peekable_quasi_closest_descendants(node, is_possible_callable_ancestor)
}

/// Returns the syntax node's descendant ink! entities of IR type `T` that either don't have any
/// ink! ancestor or only have an ink! contract entity between them and the current node.
pub fn ink_contract_wrappable_quasi_closest_descendants<T>(
    node: &SyntaxNode,
) -> impl Iterator<Item = T>
where
    T: FromSyntax + FromInkAttribute,
{
    ink_peekable_quasi_closest_descendants(node, |attr| {
        *attr.kind() == InkAttributeKind::Macro(InkMacroKind::Contract)
    })
}

/// Returns the syntax node's descendant ink! entities of IR type `T` that either don't have any
/// ink! ancestor or only have ink! entities that satisfy a "peekable" predicate between them and the current node.
pub fn ink_peekable_quasi_closest_descendants<T, F>(
    node: &SyntaxNode,
    is_peekable_ancestor: F,
) -> impl Iterator<Item = T>
where
    T: FromSyntax + FromInkAttribute,
    F: Fn(&InkAttribute) -> bool,
{
    ink_attrs_closest_descendants(node)
        .flat_map(move |attr| {
            if T::can_cast(&attr) {
                vec![T::cast(attr).expect("Should be able to cast")]
            } else if is_peekable_ancestor(&attr) {
                ink_attrs_closest_descendants(
                    <InkAttrData<ast::Impl> as From<_>>::from(attr).parent_syntax(),
                )
                .filter_map(T::cast)
                .collect()
            } else {
                Vec::new()
            }
        })
        // Deduplicate by wrapped syntax node.
        .unique_by(|item| item.syntax().to_owned())
}

/// Returns the syntax node's descendant ink! impl items that don't have any
/// ink! ancestor between them and the current node.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
pub fn ink_impl_closest_descendants(node: &SyntaxNode) -> impl Iterator<Item = InkImpl> {
    node.children()
        .filter_map(ast::Impl::cast)
        // `impl` children.
        .map(|item| item.syntax().to_owned())
        .chain(ink_attrs_closest_descendants(node).filter_map(|attr| {
            // ink! impl annotated closest descendants or an `impl` item annotated with ink! namespace.
            if is_possible_callable_ancestor(&attr) {
                parent_ast_item(attr.syntax()).map(|item| item.syntax().to_owned())
            } else if Constructor::can_cast(&attr) {
                // impl parent of ink! constructor closest descendant.
                Constructor::cast(attr)
                    .expect("Should be able to cast")
                    .impl_item()
                    .map(|item| item.syntax().to_owned())
            } else if Message::can_cast(&attr) {
                // impl parent of ink! message closest descendant.
                Message::cast(attr)
                    .expect("Should be able to cast")
                    .impl_item()
                    .map(|item| item.syntax().to_owned())
            } else {
                None
            }
        }))
        .filter_map(InkImpl::cast)
        // Deduplicate by wrapped syntax node.
        .unique_by(|item| item.syntax().to_owned())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use quote::quote;
    use test_utils::quote_as_str;

    #[test]
    fn ink_attrs_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                1,
            ),
            (
                parse_first_ast_node_of_type::<ast::Fn>(quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                })
                .syntax(),
                2,
            ),
        ] {
            assert_eq!(ink_attrs(node).count(), n_attrs);
        }
    }

    #[test]
    fn ink_attrs_descendants_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                0, // `mod` has no ink! descendants.
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            field_1: i32,
                            field_2: bool,
                        }

                        impl MyContract {
                            #[ink(message)]
                            #[ink(payable, default, selector=1)]
                            pub fn my_message(&self) {}
                        }
                    }
                })
                .syntax(),
                4, // i.e `#[ink(event)]`, `#[ink(topic)]`, `#[ink(message)]` and `#[ink(payable, default, selector=1)]`.
            ),
        ] {
            assert_eq!(ink_attrs_descendants(node).count(), n_attrs);
        }
    }

    #[test]
    fn ink_attrs_closest_descendants_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                0, // contract has no ink! descendants.
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            field_1: i32,
                            field_2: bool,
                        }

                        impl MyContract {
                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                })
                .syntax(),
                2, // i.e only `#[ink(event)]` and `#[ink(message)]` but not `#[ink(topic)]`.
            ),
        ] {
            assert_eq!(ink_attrs_closest_descendants(node).count(), n_attrs);
        }
    }

    #[test]
    fn ink_attrs_in_scope_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                1, // `#[ink::contract]`.
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            field_1: i32,
                            field_2: bool,
                        }

                        impl MyContract {
                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                })
                .syntax(),
                4, // i.e only `#[ink(event)]`, `#[ink(topic)]` and `#[ink(message)]`.
            ),
        ] {
            assert_eq!(ink_attrs_in_scope(node).count(), n_attrs);
        }
    }

    #[test]
    fn ink_attrs_ancestors_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                0, // `mod` has no ancestors.
            ),
            (
                parse_first_ast_node_of_type::<ast::RecordField>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            field_1: i32,
                            field_2: bool,
                        }
                    }
                })
                .syntax(),
                2, // i.e `#[ink(event)]` and `#[ink::contract]` are ancestors of `field_1`.
            ),
        ] {
            assert_eq!(ink_attrs_ancestors(node).count(), n_attrs);
        }
    }

    #[test]
    fn ink_attrs_closest_ancestors_works() {
        for (node, n_attrs) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                0, // `mod` has no ancestors.
            ),
            (
                parse_first_ast_node_of_type::<ast::RecordField>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(event)]
                        pub struct MyEvent {
                            #[ink(topic)]
                            field_1: i32,
                            field_2: bool,
                        }
                    }
                })
                .syntax(),
                1, // i.e only `#[ink(event)]` (but not and `#[ink::contract]`) is the closest ancestors for `field_1`.
            ),
        ] {
            assert_eq!(ink_attrs_closest_ancestors(node).count(), n_attrs);
        }
    }

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

    #[test]
    fn ink_args_works() {
        for (node, n_args) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                0,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                0, // ink! contract has no args.
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                    mod my_contract {}
                })
                .syntax(),
                2, // i.e `env=my::env::Types` and `keep_attr="foo,bar"`.
            ),
            (
                parse_first_ast_node_of_type::<ast::Fn>(quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                })
                .syntax(),
                4, // i.e `message`, `payable`, `default`, and `selector=1`.
            ),
        ] {
            assert_eq!(ink_args(node).count(), n_args);
        }
    }

    #[test]
    fn ink_arg_by_kind_works() {
        for (node, arg_kind, is_expected) in [
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    mod my_contract {}
                })
                .syntax(),
                InkArgKind::Env,
                false,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                })
                .syntax(),
                InkArgKind::Env,
                false,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                    mod my_contract {}
                })
                .syntax(),
                InkArgKind::Env,
                true,
            ),
            (
                parse_first_ast_node_of_type::<ast::Module>(quote_as_str! {
                    #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                    mod my_contract {}
                })
                .syntax(),
                InkArgKind::Namespace,
                false,
            ),
            (
                parse_first_ast_node_of_type::<ast::Fn>(quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                })
                .syntax(),
                InkArgKind::Message,
                true,
            ),
            (
                parse_first_ast_node_of_type::<ast::Fn>(quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                })
                .syntax(),
                InkArgKind::Selector,
                true,
            ),
            (
                parse_first_ast_node_of_type::<ast::Fn>(quote_as_str! {
                    #[ink(message)]
                    #[ink(payable, default, selector=1)]
                    pub fn my_message(&self) {}
                })
                .syntax(),
                InkArgKind::Constructor,
                false,
            ),
        ] {
            assert_eq!(ink_arg_by_kind(node, arg_kind).is_some(), is_expected);
        }
    }

    #[test]
    fn ink_callable_closest_descendants_works() {
        for (code, n_constructors, n_messages) in [
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                },
                0,
                0,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}
                        }
                    }
                },
                1,
                0,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                0,
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }

                        impl Mytrait for MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor2() -> Self {}

                            #[ink(message)]
                            pub fn my_message2(&self) {}
                        }
                    }
                },
                2,
                2,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(namespace="my_namespace")]
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                1,
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(impl)]
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                1,
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(impl, namespace="my_namespace")]
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                1,
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {

                        #[ink::test]
                        fn it_works() {
                            impl MyContract {
                                #[ink(constructor)]
                                pub fn my_constructor() -> Self {}

                                #[ink(message)]
                                pub fn my_message(&self) {}
                            }
                        }
                    }
                },
                // Only `impl` and `namespace` attributes are peeked through.
                0,
                0,
            ),
        ] {
            // Parse contract `mod` item.
            let module: ast::Module = parse_first_ast_node_of_type(code);

            // Check number of constructors.
            assert_eq!(
                ink_callable_closest_descendants::<Constructor>(module.syntax()).count(),
                n_constructors,
                "constructor: {}",
                code
            );
            // Check number of messages.
            assert_eq!(
                ink_callable_closest_descendants::<Message>(module.syntax()).count(),
                n_messages,
                "message: {}",
                code
            );
        }
    }

    #[test]
    fn ink_impl_closest_descendants_works() {
        for (code, n_impls) in [
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {}
                },
                0,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                        }
                    }
                },
                0, // No ink! constructors or ink! messages.
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}
                        }
                    }
                },
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }

                        impl Mytrait for MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor2() -> Self {}

                            #[ink(message)]
                            pub fn my_message2(&self) {}
                        }
                    }
                },
                2,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(namespace="my_namespace")]
                        impl MyContract {
                            #[ink(constructor)]
                            pub fn my_constructor() -> Self {}

                            #[ink(message)]
                            pub fn my_message(&self) {}
                        }
                    }
                },
                1,
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(impl)]
                        impl MyContract {
                        }
                    }
                },
                1, // empty `impl` block is valid if annotated with ink! impl attribute.
            ),
            (
                quote_as_str! {
                    #[ink::contract]
                    mod my_contract {
                        #[ink(impl, namespace="my_namespace")]
                        impl MyContract {
                        }
                    }
                },
                1, // empty `impl` block is valid if annotated with ink! impl attribute.
            ),
        ] {
            // Parse contract `mod` item.
            let module: ast::Module = parse_first_ast_node_of_type(code);

            // Check number of constructors.
            assert_eq!(
                ink_impl_closest_descendants(module.syntax()).count(),
                n_impls,
                "impls: {}",
                code
            );
        }
    }
}
