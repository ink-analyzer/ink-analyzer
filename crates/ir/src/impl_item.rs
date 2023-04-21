//! ink! contract `impl` IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Impl as ASTImpl;

use crate::{Constructor, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message};

/// An ink! contract `impl` block.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Impl {
    /// ink! attribute IR data.
    #[arg_kind(Impl)]
    ink_attr: InkAttrData<ASTImpl>,
    /// List of top level ink! constructor items.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// List of top level ink! message items.
    #[arg_kind(Message)]
    messages: Vec<Message>,
}

impl Impl {
    /// Returns the `impl` item (if any) for the ink! `contract` implementation.
    pub fn impl_item(&self) -> Option<&ASTImpl> {
        self.ink_attr.parent_ast()
    }

    /// Returns the `constructor`s for the ink! `contract` implementation.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the `message`s for the ink! `contract` implementation.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}
