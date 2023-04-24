//! ink! storage item IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Struct;

use crate::{AsInkStruct, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! storage item.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct StorageItem {
    /// ink! attribute IR data.
    #[macro_kind(StorageItem)]
    ink_attr: InkAttrData<Struct>,
}

impl AsInkStruct for StorageItem {
    fn struct_item(&self) -> Option<&Struct> {
        self.ink_attr.parent_ast()
    }
}
