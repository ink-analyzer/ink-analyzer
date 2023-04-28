//! ink! storage item IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast::Adt;

use crate::{utils, FromInkAttribute, FromSyntax, InkArg, InkArgKind, InkAttrData, InkAttribute};

/// An ink! storage item.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct StorageItem {
    /// ink! attribute IR data.
    #[macro_kind(StorageItem)]
    ink_attr: InkAttrData<Adt>,
}

impl StorageItem {
    /// Returns the ink! derive argument (if any) for the ink! storage item.
    pub fn derive_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Derive)
    }

    /// Returns the `adt` (i.e `enum`, `struct` or `union`) item (if any) for the ink! storage item.
    pub fn adt(&self) -> Option<&Adt> {
        self.ink_attr.parent_ast()
    }
}
