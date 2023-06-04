//! ink! storage diagnostics.

use ink_analyzer_ir::Storage;

use super::utils;
use crate::Diagnostic;

const STORAGE_SCOPE_NAME: &str = "storage";

/// Runs all ink! storage diagnostics.
///
/// The entry point for finding ink! storage semantic rules is the storage module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81-L101>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, storage: &Storage) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, storage);

    // Ensures that ink! storage is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L94>.
    if let Some(diagnostic) = utils::ensure_pub_struct(storage, STORAGE_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! storage is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L377-L379>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L28-L29>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L74>.
    if let Some(diagnostic) = utils::ensure_contract_parent(storage, STORAGE_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! storage has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, storage, STORAGE_SCOPE_NAME);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{FromInkAttribute, InkArgKind, InkAttributeKind, InkFile, IsInkEntity};
    use quote::quote;
    use test_utils::quote_as_str;

    fn parse_first_storage_definition(code: &str) -> Storage {
        Storage::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Storage))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! storage definitions used for positive(`works`) tests for ink! storaage verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L132-L138>.
    macro_rules! valid_storage {
        () => {
            [quote! {
                #[ink(storage)]
                pub struct MyContract {
                    field_1: i32,
                    field_2: bool,
                }
            }]
            // Wrap in contract for context sensitive tests.
            .map(|items| {
                quote! {
                    #[ink::contract]
                    mod my_contract {
                        #items
                    }
                }
            })
        };
    }

    #[test]
    fn pub_struct_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let result = utils::ensure_pub_struct(&storage, STORAGE_SCOPE_NAME);
            assert!(result.is_none(), "storage: {}", code);
        }
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
            let storage = parse_first_storage_definition(quote_as_str! {
                #[ink(storage)]
                #vis struct MyContract {
                    value: bool,
                }
            });

            let result = utils::ensure_pub_struct(&storage, STORAGE_SCOPE_NAME);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn contract_parent_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let result = utils::ensure_contract_parent(&storage, STORAGE_SCOPE_NAME);
            assert!(result.is_none(), "storage: {}", code);
        }
    }

    #[test]
    fn non_contract_parent_fails() {
        for code in [
            // Unannotated parent.
            quote_as_str! {
                mod my_contract {
                    #[ink(storage)]
                    pub struct MyContract {
                        value: bool,
                    }
                }
            },
            // Contract ancestor.
            quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    mod my_storage_mod {
                        #[ink(storage)]
                        pub struct MyContract {
                            value: bool,
                        }
                    }
                }
            },
        ] {
            let storage = parse_first_storage_definition(code);

            let result = utils::ensure_contract_parent(&storage, STORAGE_SCOPE_NAME);
            assert!(result.is_some());
            assert_eq!(result.unwrap().severity, Severity::Error);
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_no_ink_descendants(&mut results, &storage, STORAGE_SCOPE_NAME);
            assert!(results.is_empty(), "storage: {}", code);
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let storage = parse_first_storage_definition(quote_as_str! {
            #[ink(storage)]
            struct MyContract {
                #[ink(topic)]
                value: bool,
            }
        });

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &storage, STORAGE_SCOPE_NAME);
        assert_eq!(results.len(), 1);
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            1
        );
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L130-L140>.
    fn compound_diagnostic_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &storage);
            assert!(results.is_empty(), "storage: {}", code);
        }
    }
}
