//! ink! IR utilities.

use itertools::Itertools;
use ra_ap_syntax::{ast, AstNode, SyntaxKind, SyntaxNode};

use crate::{
    Constructor, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute,
    InkAttributeKind, InkImpl, InkImplItem, Message,
};

/// Casts a syntax node to an ink! attribute (if possible).
fn ink_attribute_from_node(node: SyntaxNode) -> Option<InkAttribute> {
    ast::Attr::cast(node).and_then(InkAttribute::cast)
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
/// that don't have any ink! ancestor between them and the current node.
pub fn ink_attrs_closest_descendants(node: &SyntaxNode) -> Vec<InkAttribute> {
    // Simply calling children on the syntax node directly would include the syntax node's own ink! attributes.
    // So we get the non-attribute children first and then either get their ink! attributes or return them if they have none.
    node.children()
        .filter(|child| child.kind() != SyntaxKind::ATTR)
        .flat_map(|child| {
            let child_ink_attrs = ink_attrs(&child);
            if !child_ink_attrs.is_empty() {
                // Return child ink! attributes (if any).
                child_ink_attrs
            } else {
                // Otherwise recurse on children with no ink! attributes.
                ink_attrs_closest_descendants(&child)
            }
        })
        .collect()
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
/// that don't have any ink! descendant between them and the current node.
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
pub fn parent_ast_item(node: &SyntaxNode) -> Option<ast::Item> {
    let parent = node.parent()?;
    match ast::Item::cast(parent.clone()) {
        Some(item) => Some(item),
        None => parent_ast_item(&parent),
    }
}

/// Returns the syntax node's descendant ink! entities of IR type `T`.
pub fn ink_descendants<T>(node: &SyntaxNode) -> Vec<T>
where
    T: FromInkAttribute,
{
    ink_attrs_descendants(node)
        .into_iter()
        .filter_map(T::cast)
        .collect()
}

/// Returns the syntax node's descendant ink! entities of IR type `T` that don't have any
/// ink! ancestor between them and the current node.
pub fn ink_closest_descendants<T>(node: &SyntaxNode) -> Vec<T>
where
    T: FromInkAttribute,
{
    ink_attrs_closest_descendants(node)
        .into_iter()
        .filter_map(T::cast)
        .collect()
}

/// Returns the syntax node's parent ink! entity of IR type `T` (if any).
pub fn ink_parent<T>(node: &SyntaxNode) -> Option<T>
where
    T: FromInkAttribute,
{
    parent_ast_item(node)
        .and_then(|parent| ink_attrs(parent.syntax()).into_iter().find_map(T::cast))
}

/// Returns the syntax node's ancestor ink! entities of IR type `T`.
pub fn ink_ancestors<T>(node: &SyntaxNode) -> Vec<T>
where
    T: FromInkAttribute,
{
    ink_attrs_ancestors(node)
        .into_iter()
        .filter_map(T::cast)
        .collect()
}

/// Returns the syntax node's ancestor ink! entities of IR type `T` that don't have any
/// ink! descendant between them and the current node.
pub fn ink_closest_ancestors<T>(node: &SyntaxNode) -> Vec<T>
where
    T: FromInkAttribute,
{
    ink_attrs_closest_ancestors(node)
        .into_iter()
        .filter_map(T::cast)
        .collect()
}

/// Returns ink! arguments of the syntax node.
pub fn ink_args(node: &SyntaxNode) -> Vec<InkArg> {
    ink_attrs(node)
        .iter()
        .flat_map(|attr| attr.args())
        .cloned()
        .collect()
}

/// Returns ink! arguments of a specific kind (if any) for the syntax node.
pub fn ink_args_by_kind(node: &SyntaxNode, kind: InkArgKind) -> Vec<InkArg> {
    ink_attrs(node)
        .iter()
        .flat_map(|attr| attr.args().iter().filter(|arg| *arg.kind() == kind))
        .cloned()
        .collect()
}

/// Returns ink! argument of a specific kind (if any) for the syntax node.
pub fn ink_arg_by_kind(node: &SyntaxNode, kind: InkArgKind) -> Option<InkArg> {
    ink_attrs(node)
        .iter()
        .find_map(|attr| attr.args().iter().find(|arg| *arg.kind() == kind))
        .cloned()
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
pub fn ink_callable_closest_descendants<T>(node: &SyntaxNode) -> Vec<T>
where
    T: FromSyntax + FromInkAttribute + InkImplItem,
{
    ink_attrs_closest_descendants(node)
        .into_iter()
        .flat_map(|attr| {
            if T::can_cast(&attr) {
                vec![T::cast(attr).expect("Should be able to cast")]
            } else if is_possible_callable_ancestor(&attr) {
                ink_attrs_closest_descendants(
                    <InkAttrData<ast::Impl> as From<_>>::from(attr).parent_syntax(),
                )
                .into_iter()
                .filter_map(T::cast)
                .collect()
            } else {
                Vec::new()
            }
        })
        // Deduplicate by wrapped syntax node.
        .unique_by(|item| item.syntax().to_owned())
        .collect()
}

/// Returns the syntax node's descendant ink! impl items that don't have any
/// ink! ancestor between them and the current node.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
pub fn ink_impl_closest_descendants(node: &SyntaxNode) -> Vec<InkImpl> {
    node.children()
        .filter_map(ast::Impl::cast)
        // `impl` children.
        .map(|item| item.syntax().to_owned())
        .chain(
            ink_attrs_closest_descendants(node)
                .iter()
                .filter_map(|attr| {
                    // ink! impl annotated closest descendants or an `impl` item annotated with ink! namespace.
                    if is_possible_callable_ancestor(attr) {
                        parent_ast_item(attr.syntax()).map(|item| item.syntax().to_owned())
                    } else if Constructor::can_cast(attr) {
                        // impl parent of ink! constructor closest descendant.
                        Constructor::cast(attr.to_owned())
                            .expect("Should be able to cast")
                            .impl_item()
                            .map(|item| item.syntax().to_owned())
                    } else if Message::can_cast(attr) {
                        // impl parent of ink! message closest descendant.
                        Message::cast(attr.to_owned())
                            .expect("Should be able to cast")
                            .impl_item()
                            .map(|item| item.syntax().to_owned())
                    } else {
                        None
                    }
                }),
        )
        .filter_map(InkImpl::cast)
        // Deduplicate by wrapped syntax node.
        .unique_by(|item| item.syntax().to_owned())
        .collect()
}

/// Quasi-quotation macro that accepts input like the `quote!` macro
/// but returns a string slice (`&str`) instead of a `TokenStream`.
#[macro_export]
macro_rules! quote_as_str {
    ($($tt:tt)*) => {
        quote::quote!($($tt)*).to_string().as_str()
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use quote::quote;

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
            assert_eq!(ink_attrs(node).len(), n_attrs);
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
            assert_eq!(ink_attrs_descendants(node).len(), n_attrs);
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
            assert_eq!(ink_attrs_closest_descendants(node).len(), n_attrs);
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
            assert_eq!(ink_attrs_in_scope(node).len(), n_attrs);
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
            assert_eq!(ink_attrs_ancestors(node).len(), n_attrs);
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
            assert_eq!(ink_attrs_closest_ancestors(node).len(), n_attrs);
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
            assert_eq!(ink_args(node).len(), n_args);
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
                ink_callable_closest_descendants::<Constructor>(module.syntax()).len(),
                n_constructors,
                "constructor: {}",
                code
            );
            // Check number of messages.
            assert_eq!(
                ink_callable_closest_descendants::<Message>(module.syntax()).len(),
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
                ink_impl_closest_descendants(module.syntax()).len(),
                n_impls,
                "impls: {}",
                code
            );
        }
    }
}
