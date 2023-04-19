//! ink! contract `storage` IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{AsInkStruct, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! contract's `storage`.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct Storage {
    /// ink! attribute IR data.
    #[arg_kind(Storage)]
    ink_attr: InkAttrData<Struct>,
}

impl AsInkStruct for Storage {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}
