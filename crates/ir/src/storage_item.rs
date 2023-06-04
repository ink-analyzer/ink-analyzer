//! ink! storage item IR.

use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ra_ap_syntax::ast;

use crate::traits::{FromInkAttribute, FromSyntax};
use crate::tree::utils;
use crate::{InkArg, InkArgKind, InkAttrData, InkAttribute};

/// An ink! storage item.
#[derive(Debug, Clone, PartialEq, Eq, FromInkAttribute, FromSyntax)]
pub struct StorageItem {
    /// ink! attribute IR data.
    #[macro_kind(StorageItem)]
    ink_attr: InkAttrData<ast::Adt>,
}

impl StorageItem {
    /// Returns the ink! derive argument (if any) for the ink! storage item.
    pub fn derive_arg(&self) -> Option<InkArg> {
        utils::ink_arg_by_kind(self.syntax(), InkArgKind::Derive)
    }

    /// Returns the `adt` (i.e `enum`, `struct` or `union`) item (if any) for the ink! storage item.
    pub fn adt(&self) -> Option<&ast::Adt> {
        self.ink_attr.parent_ast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use quote::quote;
    use test_utils::quote_as_str;

    #[test]
    fn cast_works() {
        for code in [
            quote! {
                struct MyStorageItem {
                }
            },
            quote! {
                enum MyStorageItem {
                }
            },
            quote! {
                union MyStorageItem {
                }
            },
        ] {
            let ink_attr = parse_first_ink_attribute(quote_as_str! {
                #[ink::storage_item(derive=false)]
                #code
            });

            let storage_item = StorageItem::cast(ink_attr).unwrap();

            // 1 `derive` argument exists.
            assert!(storage_item.derive_arg().is_some());

            // ADT item exists.
            assert!(storage_item.adt().is_some());
        }
    }
}
