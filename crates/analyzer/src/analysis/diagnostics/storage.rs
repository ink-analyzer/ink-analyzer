//! ink! storage diagnostics.

use ink_analyzer_ir::Storage;

use super::utils;
use crate::Diagnostic;

/// Runs all ink! storage diagnostics.
///
/// The entry point for finding ink! storage semantic rules is the storage module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81-L101>.
pub fn diagnostics(storage: &Storage) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(storage));

    // Ensure ink! storage is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L94>.
    if let Some(diagnostic) = utils::ensure_pub_struct(storage) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! storage is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L377-L379>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L28-L29>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L74>.
    if let Some(diagnostic) = utils::ensure_contract_parent(storage) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! storage has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(storage, "storage"),
    );

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{
        quote_as_str, FromInkAttribute, IRItem, InkArgKind, InkAttributeKind, InkFile,
    };
    use quote::quote;

    fn parse_first_storage_item(code: &str) -> Storage {
        Storage::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Storage))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn pub_struct_works() {
        let storage = parse_first_storage_item(quote_as_str! {
            #[ink(storage)]
            pub struct Flipper {
                value: bool,
            }
        });

        let result = utils::ensure_pub_struct(&storage);
        assert!(result.is_none());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L207-L219>.
    fn non_pub_struct_fails() {
        for vis in vec![
            quote! {}, // no visibility
            quote! { crate },
            quote! { pub(crate) },
            quote! { pub(self) },
            quote! { pub(super) },
            quote! { pub(in my::path) },
        ] {
            let storage = parse_first_storage_item(quote_as_str! {
                #[ink(storage)]
                #vis struct Flipper {
                    value: bool,
                }
            });

            let result = utils::ensure_pub_struct(&storage);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn contract_parent_works() {
        let storage = parse_first_storage_item(quote_as_str! {
            #[ink::contract]
            mod flipper {
                #[ink(storage)]
                pub struct Flipper {
                    value: bool,
                }
            }
        });

        let result = utils::ensure_contract_parent(&storage);
        assert!(result.is_none());
    }

    #[test]
    fn non_contract_parent_fails() {
        let storage = parse_first_storage_item(quote_as_str! {
            mod flipper {
                #[ink(storage)]
                pub struct Flipper {
                    value: bool,
                }
            }
        });

        let result = utils::ensure_contract_parent(&storage);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn contract_ancestor_fails() {
        let storage = parse_first_storage_item(quote_as_str! {
            #[ink::contract]
            mod flipper {
                mod storage_container {
                    #[ink(storage)]
                    pub struct Flipper {
                        value: bool,
                    }
                }
            }
        });

        let result = utils::ensure_contract_parent(&storage);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn no_ink_descendants_works() {
        let storage = parse_first_storage_item(quote_as_str! {
            #[ink(storage)]
            struct Flipper {
                value: bool,
            }
        });

        let results = utils::ensure_no_ink_descendants(&storage, "storage");
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let storage = parse_first_storage_item(quote_as_str! {
            #[ink(storage)]
            struct Flipper {
                #[ink(topic)]
                value: bool,
            }
        });

        let results = utils::ensure_no_ink_descendants(&storage, "storage");
        assert_eq!(results.len(), 1);
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            1
        );
    }
}
