//! ink! storage diagnostics.

use ink_analyzer_ir::Storage;

use crate::analysis::diagnostics::common;
use crate::{Diagnostic, Version};

const SCOPE_NAME: &str = "storage";

/// Runs all ink! storage diagnostics.
///
/// The entry point for finding ink! storage semantic rules is the storage module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81-L101>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, storage: &Storage, version: Version) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, storage, version);

    // Ensures that ink! storage is a `struct` with `pub` visibility, see `utils::ensure_pub_struct` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L81>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L94>.
    if let Some(diagnostic) = common::ensure_pub_struct(storage, SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! storage is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L377-L379>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L28-L29>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L64-L74>.
    if let Some(diagnostic) = common::ensure_contract_parent(storage, SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! storage has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    common::ensure_no_ink_descendants(results, storage, SCOPE_NAME, false);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_storage_definition(code: &str) -> Storage {
        parse_first_ink_entity_of_type(code)
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

            let result = common::ensure_pub_struct(&storage, SCOPE_NAME);
            assert!(result.is_none(), "storage: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L207-L219>.
    fn non_pub_struct_fails() {
        for (vis, expected_quickfixes) in vec![
            (
                quote! {},
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-struct"),
                        end_pat: Some("<-struct"),
                    }],
                }],
            ), // no visibility
            (
                quote! { pub(crate) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(crate)"),
                        end_pat: Some("pub(crate)"),
                    }],
                }],
            ),
            (
                quote! { pub(self) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(self)"),
                        end_pat: Some("pub(self)"),
                    }],
                }],
            ),
            (
                quote! { pub(super) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(super)"),
                        end_pat: Some("pub(super)"),
                    }],
                }],
            ),
            (
                quote! { pub(in my::path) },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(in my::path)"),
                        end_pat: Some("pub(in my::path)"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(storage)]
                #vis struct MyContract {
                    value: bool,
                }
            };
            let storage = parse_first_storage_definition(&code);

            let result = common::ensure_pub_struct(&storage, SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some());
            assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
            // Verifies quickfixes.
            verify_actions(
                &code,
                result.as_ref().unwrap().quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn contract_parent_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let result = common::ensure_contract_parent(&storage, SCOPE_NAME);
            assert!(result.is_none(), "storage: {code}");
        }
    }

    #[test]
    fn non_contract_parent_fails() {
        for (code, expected_quickfixes) in [
            // Unannotated parent.
            (
                quote_as_pretty_string! {
                    mod my_contract {
                        #[ink(storage)]
                        pub struct MyContract {
                            value: bool,
                        }
                    }
                },
                vec![],
            ),
            // Contract ancestor.
            (
                quote_as_pretty_string! {
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
                vec![TestResultAction {
                    label: "Move item",
                    edits: vec![
                        TestResultTextRange {
                            text: "pub struct MyContract",
                            start_pat: Some("my_contract {"),
                            end_pat: Some("my_contract {"),
                        },
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("}"),
                        },
                    ],
                }],
            ),
        ] {
            let storage = parse_first_storage_definition(&code);

            let result = common::ensure_contract_parent(&storage, SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some());
            assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
            // Verifies quickfixes.
            verify_actions(
                &code,
                result
                    .as_ref()
                    .unwrap()
                    .quickfixes
                    .as_ref()
                    .unwrap_or(&vec![]),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            common::ensure_no_ink_descendants(&mut results, &storage, SCOPE_NAME, false);
            assert!(results.is_empty(), "storage: {code}");
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink(storage)]
            struct MyContract {
                #[ink(topic)]
                value: bool,
            }
        };
        let storage = parse_first_storage_definition(&code);

        let mut results = Vec::new();
        common::ensure_no_ink_descendants(&mut results, &storage, SCOPE_NAME, false);

        // Verifies diagnostics.
        assert_eq!(results.len(), 1);
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            1
        );
        // Verifies quickfixes.
        let expected_quickfixes = [vec![TestResultAction {
            label: "Remove `#[ink(topic)]`",
            edits: vec![TestResultTextRange {
                text: "",
                start_pat: Some("<-#[ink(topic)]"),
                end_pat: Some("#[ink(topic)]"),
            }],
        }]];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/storage.rs#L130-L140>.
    fn compound_diagnostic_works() {
        for code in valid_storage!() {
            let storage = parse_first_storage_definition(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &storage, Version::Legacy);
            assert!(results.is_empty(), "storage: {code}");
        }
    }
}
