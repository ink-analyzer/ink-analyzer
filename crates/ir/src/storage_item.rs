//! ink! storage item IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Adt;

use crate::{FromInkAttribute, FromSyntax, InkAttrData, InkAttribute};

/// An ink! storage item.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct StorageItem {
    /// ink! attribute IR data.
    #[macro_kind(StorageItem)]
    ink_attr: InkAttrData<Adt>,
}

impl StorageItem {
    /// Returns the `adt` (i.e `enum`, `struct` or `union`) item (if any) for the ink! storage item.
    pub fn adt(&self) -> Option<&Adt> {
        self.ink_attr.parent_ast()
    }
}
