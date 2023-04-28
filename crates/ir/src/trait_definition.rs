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
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Env)
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
