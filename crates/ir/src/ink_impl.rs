//! ink! impl IR.

use ra_ap_syntax::ast::HasName;
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

        // Only continue if the last segment is a valid.
        let target = path.segment()?;

        // Resolves the root module (i.e. the file root).
        let resolve_root_module = |node: &SyntaxNode| node.ancestors().last();
        // Resolves current module (defaults to the file root if there's no `mod` item).
        let resolve_current_module = |node: &SyntaxNode| {
            node.ancestors()
                .find(|it| ast::Module::can_cast(it.kind()))
                .or(resolve_root_module(node))
        };
        // Resolves next child module.
        let resolve_next_child_module = |root: &SyntaxNode, name: &ast::NameRef| {
            root.children().find(|it| {
                ast::Module::can_cast(it.kind())
                    && ast::Module::cast(it.clone())
                        .and_then(|module| module.name())
                        .map_or(false, |module_name| module_name.text() == name.text())
            })
        };

        // Determines the root module for target item resolution.
        // Ref: <https://doc.rust-lang.org/reference/paths.html#paths-in-expressions>.
        let resolution_root = match path.qualifier() {
            // Resolves based on qualifier.
            Some(qualifier) => {
                let mut qualifier_segments = qualifier
                    .segments()
                    // Calling segments on the qualifier appears to also include the target for some reason,
                    // so we filter it out manually.
                    .filter(|segment| *segment != target);

                // Resolves first path segment including respecting valid path qualifiers
                // (i.e. `::`, `crate`, `self`, `super`).
                // NOTE: $crate and Self aren't valid path qualifiers for our context
                // so they're are treated as module/item names.
                let mut resolution_root_option =
                    qualifier_segments.next().and_then(|root_segment| {
                        if root_segment.coloncolon_token().is_some()
                            || root_segment.crate_token().is_some()
                        {
                            // Resolve from crate root (and next path segment if any).
                            resolve_root_module(self.syntax()).and_then(|crate_root| {
                                match root_segment.coloncolon_token() {
                                    // Resolves next segment if path has `::` qualifier.
                                    Some(_) => root_segment.name_ref().and_then(|name| {
                                        resolve_next_child_module(&crate_root, &name)
                                    }),
                                    // Otherwise returns the crate root.
                                    None => Some(crate_root),
                                }
                            })
                        } else if root_segment.self_token().is_some() {
                            // Resolve from current module.
                            resolve_current_module(self.syntax())
                        } else if root_segment.super_token().is_some() {
                            // Resolve from parent module.
                            resolve_current_module(self.syntax())
                                .as_ref()
                                .and_then(SyntaxNode::parent)
                                .as_ref()
                                .and_then(resolve_current_module)
                        } else {
                            resolve_current_module(self.syntax())
                                .zip(root_segment.name_ref())
                                .and_then(|(current_module, name)| {
                                    resolve_next_child_module(&current_module, &name)
                                })
                        }
                    });

                // Resolves the remaining qualifier segments (if any).
                while let Some((node, segment)) = resolution_root_option
                    .as_ref()
                    .zip(qualifier_segments.next())
                {
                    resolution_root_option = segment
                        .name_ref()
                        .and_then(|name| resolve_next_child_module(node, &name));
                }

                resolution_root_option
            }
            // Resolves from current module if there's no specifier.
            None => resolve_current_module(self.syntax()),
        };

        resolution_root
            .map(|root_node| {
                if ast::Module::can_cast(root_node.kind()) {
                    // Use the item list as the root node for module.
                    ast::Module::cast(root_node.clone())
                        .as_ref()
                        .and_then(ast::Module::item_list)
                        .as_ref()
                        .map(AstNode::syntax)
                        .cloned()
                        .unwrap_or(root_node)
                } else {
                    // Otherwise use the root node directly (e.g. for file roots).
                    root_node
                }
            })
            .zip(target.name_ref())
            .and_then(|(root_node, target_name)| {
                root_node
                    .children()
                    .filter(|node| {
                        ast::Trait::can_cast(node.kind()) && TraitDefinition::can_cast(node)
                    })
                    .find_map(|node| {
                        ast::Trait::cast(node.clone()).filter(|trait_item| {
                            trait_item
                                .name()
                                .map_or(false, |trait_name| trait_name.text() == target_name.text())
                        })
                    })
                    .and_then(|trait_item| TraitDefinition::cast(trait_item.syntax().clone()))
            })
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
