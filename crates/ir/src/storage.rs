//! ink! storage IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, InkStruct};

/// An ink! storage item.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Storage {
    /// ink! attribute IR data.
    #[arg_kind(Storage)]
    ink_attr: InkAttrData<Struct>,
}

impl InkStruct for Storage {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}
