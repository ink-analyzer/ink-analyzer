//! ink! trait definition IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Trait;

use crate::{Constructor, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message};

/// An ink! trait definition.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct TraitDefinition {
    /// ink! attribute IR data.
    #[macro_kind(TraitDefinition)]
    ink_attr: InkAttrData<Trait>,
    /// ink! constructors.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// ink! messages.
    #[arg_kind(Message)]
    messages: Vec<Message>,
}

impl TraitDefinition {
    /// Returns the ink! `trait` item (if any) for the ink! trait definition.
    pub fn trait_item(&self) -> Option<&Trait> {
        self.ink_attr.parent_ast()
    }

    /// Returns the ink! constructors for the ink! trait definition.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! trait definition.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}
