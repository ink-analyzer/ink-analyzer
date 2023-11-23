//! ink! trait definition IR.

use ra_ap_syntax::ast;

use crate::traits::InkEntity;
use crate::{InkArg, Message};

/// An ink! trait definition.
#[ink_analyzer_macro::entity(macro_kind = TraitDefinition)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitDefinition {
    // ASTNode type.
    ast: ast::Trait,
    // ink! messages.
    messages: Vec<Message>,
}

impl_ast_type_trait!(TraitDefinition, IsInkTrait);

impl TraitDefinition {
    impl_pub_ink_arg_getter!(namespace_arg, Namespace, namespace);

    impl_pub_ink_arg_getter!(keep_attr_arg, KeepAttr, keep_attr);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::IsInkTrait;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        let node = parse_first_syntax_node(quote_as_str! {
            #[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]
            pub trait MyTrait {
                #[ink(message)]
                fn my_message(&self);

                #[ink(message)]
                fn my_message_mut(&mut self);
            }
        });

        let trait_definition = TraitDefinition::cast(node).unwrap();

        // `namespace` argument exists.
        assert!(trait_definition.namespace_arg().is_some());

        // `keep_attr` argument exists.
        assert!(trait_definition.keep_attr_arg().is_some());

        // 2 messages.
        assert_eq!(trait_definition.messages().len(), 2);

        // `trait` item exists.
        assert!(trait_definition.trait_item().is_some());
    }
}
