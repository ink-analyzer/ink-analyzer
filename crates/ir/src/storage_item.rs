//! ink! storage item IR.

use ra_ap_syntax::ast;

/// An ink! storage item.
#[ink_analyzer_macro::entity(macro_kind = StorageItem)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StorageItem {
    // ASTNode type.
    ast: ast::Adt,
}

impl StorageItem {
    impl_pub_ast_type_getter!(adt, Adt);

    impl_pub_ink_arg_getter!(derive_arg, Derive, derive);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::traits::InkEntity;
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
            let node = parse_first_syntax_node(quote_as_str! {
                #[ink::storage_item(derive=false)]
                #code
            });

            let storage_item = StorageItem::cast(node).unwrap();

            // 1 `derive` argument exists.
            assert!(storage_item.derive_arg().is_some());

            // ADT item exists.
            assert!(storage_item.adt().is_some());
        }
    }
}
