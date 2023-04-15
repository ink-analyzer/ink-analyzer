//! ink! contract message IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;
use ra_ap_syntax::SyntaxNode;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract message.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Message {
    /// ink! attribute IR data.
    #[arg_kind(Message)]
    ink_attr: InkAttrData<Fn>,
}

impl Message {
    /// Returns the fn item (if any) for the ink! contract message.
    pub fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
