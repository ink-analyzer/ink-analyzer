//! ink! trait definition IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Trait;

use crate::{
    utils, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute, InkTrait,
    Message,
};

/// An ink! trait definition.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct TraitDefinition {
    /// ink! attribute IR data.
    #[macro_kind(TraitDefinition)]
    ink_attr: InkAttrData<Trait>,
    /// ink! messages.
    #[arg_kind(Message)]
    messages: Vec<Message>,
}

impl InkTrait for TraitDefinition {
    fn trait_item(&self) -> Option<&Trait> {
        self.ink_attr.parent_ast()
    }
}

impl TraitDefinition {
    /// Returns the ink! namespace argument (if any) for the ink! trait definition.
    pub fn namespace_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Namespace)
    }

    /// Returns the ink! keep_attr argument (if any) for the ink! trait definition.
    pub fn keep_attr_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::KeepAttr)
    }

    /// Returns the ink! messages for the ink! trait definition.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::quote_as_str;
    use crate::test_utils::*;

    #[test]
    fn cast_works() {
        let ink_attr = parse_first_ink_attribute(quote_as_str! {
            #[ink::trait_definition(namespace="my_namespace", keep_attr="foo,bar")]
            pub trait MyTrait {
                #[ink(message)]
                fn my_message(&self);

                #[ink(message)]
                fn my_message_mut(&mut self);
            }
        });

        let trait_definition = TraitDefinition::cast(ink_attr).unwrap();

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
