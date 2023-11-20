//! ink! trait definition IR.

use ra_ap_syntax::ast;

use crate::traits::{InkEntity, IsInkTrait};
use crate::tree::utils;
use crate::{InkArg, InkArgKind, Message};

/// An ink! trait definition.
#[ink_analyzer_macro::entity(macro_kind = TraitDefinition)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitDefinition {
    // ASTNode type.
    ast: ast::Trait,
    // ink! messages.
    messages: Vec<Message>,
}

impl IsInkTrait for TraitDefinition {
    fn trait_item(&self) -> Option<&ast::Trait> {
        self.ast.as_ref()
    }
}

impl TraitDefinition {
    /// Returns the ink! namespace argument (if any) for the ink! trait definition.
    pub fn namespace_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Namespace)
    }

    /// Returns the ink! `keep_attr` argument (if any) for the ink! trait definition.
    pub fn keep_attr_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::KeepAttr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
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
