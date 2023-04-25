//! ink! trait definition IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Trait;

use crate::{AsInkTrait, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message};

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

impl AsInkTrait for TraitDefinition {
    fn trait_item(&self) -> Option<&Trait> {
        self.ink_attr.parent_ast()
    }
}

impl TraitDefinition {
    /// Returns the ink! messages for the ink! trait definition.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}
