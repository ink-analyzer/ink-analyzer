//! ink! impl IR.

use ink_analyzer_macro::FromSyntax;
use ra_ap_syntax::ast::HasName;
use ra_ap_syntax::{ast, AstNode, SyntaxNode};

use crate::traits::{FromInkAttribute, FromSyntax, IsInkEntity};
use crate::tree::utils;
use crate::{
    Constructor, InkArg, InkArgKind, InkAttribute, InkAttributeKind, Message, TraitDefinition,
};

/// An ink! impl block.
#[derive(Debug, Clone, PartialEq, Eq, FromSyntax)]
pub struct InkImpl {
    /// ink! constructors.
    constructors: Vec<Constructor>,
    /// ink! messages.
    messages: Vec<Message>,
    /// Syntax node for ink! impl.
    syntax: SyntaxNode,
}

impl InkImpl {
    /// Returns true if the syntax node can be converted into an ink! impl item.
    ///
    /// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L118-L216>.
    pub fn can_cast(node: &SyntaxNode) -> bool {
        // Has ink! impl attribute.
        utils::ink_attrs(node)
            .any(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
            // Is an `impl` item and has any ink! constructor or ink! message annotated descendants.
            || (ast::Impl::can_cast(node.kind())
                && utils::ink_attrs_closest_descendants(node)
                    .any(|attr| {
                        matches!(
                            attr.kind(),
                            InkAttributeKind::Arg(InkArgKind::Constructor | InkArgKind::Message)
                        )
                    }))
    }

    /// Converts a syntax node into an ink! impl item (if possible).
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(&node).then_some(Self {
            constructors: utils::ink_closest_descendants(&node).collect(),
            messages: utils::ink_closest_descendants(&node).collect(),
            syntax: node,
        })
    }

    /// Returns the `impl` item (if any) for the ink! impl.
    pub fn impl_item(&self) -> Option<ast::Impl> {
        ast::Impl::cast(self.syntax.clone())
    }

    /// Returns the trait type (if any) for the ink! impl.
    pub fn trait_type(&self) -> Option<ast::Type> {
        self.impl_item().and_then(|impl_item| impl_item.trait_())
    }

    /// Returns the ink! trait definition (if any) for the ink! impl.
    pub fn trait_definition(&self) -> Option<TraitDefinition> {
        self.trait_type()
            .and_then(|trait_type| match trait_type {
                ast::Type::PathType(path_type) => path_type.path(),
                _ => None,
            })
            .and_then(|path| {
                path.segment()
                    .and_then(|path_segment| path_segment.name_ref())
            })
            .zip(self.syntax().ancestors().last())
            .and_then(|(name_ref, source)| {
                source.children().find_map(|child| {
                    ast::Trait::cast(child.clone())
                        .filter(|trait_item| {
                            trait_item
                                .name()
                                .map_or(false, |trait_name| trait_name.text() == name_ref.text())
                        })
                        .and_then(|trait_item| {
                            utils::ink_attrs(trait_item.syntax())
                                .find_map(|attr| TraitDefinition::cast(attr.clone()))
                        })
                })
            })
    }

    /// Returns the ink! impl attribute (if any).
    pub fn impl_attr(&self) -> Option<InkAttribute> {
        self.tree()
            .ink_attrs()
            .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Impl))
    }

    /// Returns the ink! impl namespace argument (if any).
    pub fn namespace_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(&self.syntax, InkArgKind::Namespace)
    }

    /// Returns the ink! constructors for the ink! impl.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! impl.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ra_ap_syntax::SourceFile;
    use test_utils::quote_as_str;

    pub fn parse_first_impl_item(code: &str) -> ast::Impl {
        SourceFile::parse(code)
            .tree()
            .syntax()
            .descendants()
            .find_map(ast::Impl::cast)
            .unwrap()
    }

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
                    impl ::my_full::long_path::MyTrait for MyContract {
                        #[ink(message, payable, default, selector=0x2)]
                        fn my_message(&self) {}
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
                    impl relative_path::MyTrait for MyContract {
                        #[ink(message)]
                        fn my_message(&self) {}
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
            let impl_item = parse_first_impl_item(code);

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
