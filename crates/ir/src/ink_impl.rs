//! ink! impl IR.

use ra_ap_syntax::{ast, AstNode, SyntaxNode};

use crate::traits::InkEntity;
use crate::tree::utils;
use crate::{Constructor, InkArgKind, InkAttribute, InkAttributeKind, Message, TraitDefinition};

/// An ink! impl block.
#[ink_analyzer_macro::entity(call = self::can_cast)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InkImpl {
    // ASTNode type.
    ast: ast::Impl,
    // ink! constructors.
    constructors: Vec<Constructor>,
    // ink! messages.
    messages: Vec<Message>,
}

// Returns true if the syntax node can be converted into an ink! impl item.
//
// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
fn can_cast(node: &SyntaxNode) -> bool {
    // Has ink! impl attribute.
    utils::ink_attrs(node)
        .any(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
        // Is an `impl` item and has any ink! constructor or ink! message annotated descendants.
        || (ast::Impl::can_cast(node.kind())
        && utils::ink_attrs_closest_descendants(node)
        .any(|attr| {
            matches!(attr.kind(), InkAttributeKind::Arg(InkArgKind::Constructor | InkArgKind::Message))
        }))
}

impl InkImpl {
    impl_pub_ast_type_getter!(impl_item, Impl);

    /// Returns the trait type (if any).
    pub fn trait_type(&self) -> Option<ast::Type> {
        self.impl_item().and_then(ast::Impl::trait_)
    }

    /// Returns the ink! impl attribute (if any).
    pub fn impl_attr(&self) -> Option<InkAttribute> {
        self.tree()
            .ink_attrs()
            .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
    }

    impl_pub_ink_arg_getter!(namespace_arg, Namespace, namespace);

    /// Returns the ink! trait definition (if any).
    pub fn trait_definition(&self) -> Option<TraitDefinition> {
        let path = match self.trait_type()? {
            ast::Type::PathType(path_type) => path_type.path(),
            _ => None,
        }?;

        // Resolves the trait definition (if any) based on the path.
        utils::resolve_item::<ast::Trait, _>(&path, self.syntax(), Some(TraitDefinition::can_cast))
            .and_then(|trait_item| TraitDefinition::cast(trait_item.syntax().clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_first_ast_node_of_type;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for (
            code,
            has_impl_attr,
            has_namespace,
            n_constructors,
            n_messages,
            has_trait_definition,
        ) in [
            (
                quote_as_str! {
                    impl MyContract {
                        #[ink(constructor, payable, default, selector=_)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message, payable, default, selector=_)]
                        pub fn my_message(&self) {}
                    }
                },
                false,
                false,
                1,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    impl MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    impl self::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    impl ::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    mod traits {
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self);
                        }
                    }

                    impl traits::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    mod traits {
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self);
                        }
                    }

                    impl crate::traits::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    mod traits {
                        #[ink::trait_definition]
                        pub trait MyTrait {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self);
                        }
                    }

                    impl ::traits::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self) {}
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    #[ink::contract]
                    mod my_contract {
                        impl super::MyTrait for MyContract {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self) {}
                        }
                    }
                },
                false,
                false,
                0,
                1,
                true,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    #[ink::contract]
                    mod my_contract {
                        impl MyTrait for MyContract {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self) {}
                        }
                    }
                },
                false,
                false,
                0,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink::trait_definition]
                    pub trait MyTrait {
                        #[ink(message, payable, default, selector=1)]
                        fn my_message(&self);
                    }

                    #[ink::contract]
                    mod my_contract {
                        impl MyTrait for MyContract {
                            #[ink(message, payable, default, selector=1)]
                            fn my_message(&self) {}
                        }
                    }
                },
                false,
                false,
                0,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(namespace="my_namespace")]
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                false,
                true,
                1,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(impl)]
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                true,
                false,
                1,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(impl, namespace="my_namespace")]
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {}

                        #[ink(message)]
                        pub fn my_message(&self) {}
                    }
                },
                true,
                true,
                1,
                1,
                false,
            ),
            (
                quote_as_str! {
                    #[ink(impl)]
                    impl MyContract {
                    }
                },
                true,
                false,
                0,
                0,
                false,
            ),
        ] {
            let impl_item: ast::Impl = parse_first_ast_node_of_type(code);
            let ink_impl = InkImpl::cast(impl_item.syntax().clone()).unwrap();

            // ink! impl attribute exists.
            assert_eq!(ink_impl.impl_attr().is_some(), has_impl_attr);

            // `namespace` argument exists.
            assert_eq!(ink_impl.namespace_arg().is_some(), has_namespace);

            // number of constructors.
            assert_eq!(ink_impl.constructors().len(), n_constructors);

            // number of messages.
            assert_eq!(ink_impl.messages().len(), n_messages);

            // `impl` item exists.
            assert!(ink_impl.impl_item().is_some());

            // ink! trait definition for `impl` item exists.
            assert_eq!(ink_impl.trait_definition().is_some(), has_trait_definition);
        }
    }
}
