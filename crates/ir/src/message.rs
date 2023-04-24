//! ink! message IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Fn;

use crate::{AsInkFn, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! message.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Message {
    /// ink! attribute IR data.
    #[arg_kind(Message)]
    ink_attr: InkAttrData<Fn>,
}

impl AsInkFn for Message {
    fn fn_item(&self) -> Option<&Fn> {
        self.ink_attr.parent_ast()
    }
}
