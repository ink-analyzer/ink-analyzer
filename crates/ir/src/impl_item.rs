//! ink! impl IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::{Constructor, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message};

/// An ink! impl block.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Impl {
    /// ink! attribute IR data.
    #[arg_kind(Impl)]
    ink_attr: InkAttrData<ast::Impl>,
    /// ink! constructors.
    #[arg_kind(Constructor)]
    constructors: Vec<Constructor>,
    /// ink! messages.
    #[arg_kind(Message)]
    messages: Vec<Message>,
}

impl Impl {
    /// Returns the ink! `impl` item (if any) for the ink! contract implementation.
    pub fn impl_item(&self) -> Option<&ast::Impl> {
        self.ink_attr.parent_ast()
    }

    /// Returns the ink! constructors for the ink! contract implementation.
    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }

    /// Returns the ink! messages for the ink! contract implementation.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }
}
