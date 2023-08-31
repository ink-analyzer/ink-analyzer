//! ink! storage item diagnostics.

use ink_analyzer_ir::{FromInkAttribute, FromSyntax, StorageItem};

use super::utils;
use crate::{Action, Diagnostic, Severity};

const STORAGE_ITEM_SCOPE_NAME: &str = "storage_item";

/// Runs all ink! storage item diagnostics.
///
/// The entry point for finding ink! storage item semantic rules is the `storage_item` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/mod.rs#L33-L54>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, storage_item: &StorageItem) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, storage_item);

    // Ensures that ink! storage item is applied to an `adt` (i.e `enum`, `struct` or `union`) item., see `ensure_adt` doc.
    if let Some(diagnostic) = ensure_adt(storage_item) {
        results.push(diagnostic);
    }

    // Ensures that ink! storage item has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, storage_item, STORAGE_ITEM_SCOPE_NAME);
}

/// Ensures that ink! storage item is an `adt` (i.e `enum`, `struct` or `union`) item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/mod.rs#L28>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/mod.rs#L125-L128>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/storage_item/mod.rs#L63-L81>.
///
/// Ref: <https://github.com/dtolnay/syn/blob/2.0.15/src/derive.rs#L4-L30>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/codegen/src/generator/storage_item.rs#L50-L54>.
fn ensure_adt(storage_item: &StorageItem) -> Option<Diagnostic> {
    storage_item.adt().is_none().then_some(Diagnostic {
        message: format!(
            "`{}` can only be applied to an `enum`, `struct` or `union` item.",
            storage_item.ink_attr().syntax()
        ),
        range: storage_item.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: Some(vec![Action::remove_attribute(storage_item.ink_attr())]),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::verify_actions;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use ink_analyzer_ir::{InkAttributeKind, InkFile, InkMacroKind, IsInkEntity};
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_storage_item(code: &str) -> StorageItem {
        StorageItem::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::StorageItem))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn adt_works() {
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
            let storage_item = parse_first_storage_item(quote_as_str! {
                #[ink::storage_item]
                #code
            });

            let result = ensure_adt(&storage_item);
            assert!(result.is_none());
        }
    }

    #[test]
    fn non_adt_fails() {
        for code in [
            quote! {
                fn my_storage_item() {
                }
            },
            quote! {
                mod my_storage_item;
            },
            quote! {
                trait MyStorageItem {
                }
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink::storage_item]
                #code
            };
            let storage_item = parse_first_storage_item(&code);

            let result = ensure_adt(&storage_item);

            // Verifies diagnostics.
            assert!(result.is_some(), "storage item: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "storage item: {code}"
            );
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink::storage_item]`"));
            assert_eq!(&fix.edits[0].text, "");
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(
                        parse_offset_at(&code, Some("<-#[ink::storage_item]")).unwrap() as u32
                    ),
                    TextSize::from(
                        parse_offset_at(&code, Some("#[ink::storage_item]")).unwrap() as u32
                    )
                )
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let storage_item = parse_first_storage_item(quote_as_str! {
            #[ink::storage_item]
            struct MyStorageItem {
            }
        });

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &storage_item, STORAGE_ITEM_SCOPE_NAME);
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink::storage_item]
            struct MyStorageItem {
                #[ink(event)]
                field_1: (u32, bool),
                #[ink(topic)]
                field_2: String,
            }
        };
        let storage_item = parse_first_storage_item(&code);

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &storage_item, STORAGE_ITEM_SCOPE_NAME);
        // 1 diagnostics for `event` and `topic`.
        assert_eq!(results.len(), 2);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            2
        );
        // Verifies quickfixes.
        let expected_quickfixes = vec![
            vec![TestResultAction {
                label: "Remove `#[ink(event)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(event)]"),
                    end_pat: Some("#[ink(event)]"),
                }],
            }],
            vec![TestResultAction {
                label: "Remove `#[ink(topic)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(topic)]"),
                    end_pat: Some("#[ink(topic)]"),
                }],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L673-L770>.
    fn compound_diagnostic_works() {
        for code in [
            quote_as_str! {
                // Example of how to define the non-packed type.
                #[ink::storage_item]
                #[derive(Default, Debug)]
                struct NonPacked {
                    s1: Mapping<u32, u128>,
                    s2: Lazy<u128>,
                }
            },
            quote_as_str! {
                // Example of how to define the non-packed generic type.
                #[ink::storage_item(derive = false)]
                #[derive(Storable, StorableHint, StorageKey)]
                #[cfg_attr(
                    feature = "std",
                    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
                )]
                #[derive(Default, Debug)]
                struct NonPackedGeneric<T>
                where
                    T: Default + core::fmt::Debug,
                    T: ink::storage::traits::Packed,
                {
                    s1: u32,
                    s2: T,
                    s3: Mapping<u128, T>,
                }
            },
            quote_as_str! {
                // Example of how to define a complex packed type.
                #[ink::storage_item]
                #[derive(scale::Decode, scale::Encode)]
                #[cfg_attr(
                    feature = "std",
                    derive(scale_info::TypeInfo, ink::storage::traits::StorageLayout)
                )]
                #[derive(Default, Debug)]
                struct PackedComplex {
                    s1: u128,
                    s2: Vec<u128>,
                    s3: Vec<Packed>,
                }
            },
            quote_as_str! {
                // Example of how to define a complex non-packed type.
                #[ink::storage_item]
                #[derive(Default, Debug)]
                struct NonPackedComplex<KEY: StorageKey> {
                    s1: (String, u128, Packed),
                    s2: Mapping<u128, u128>,
                    s3: Lazy<u128>,
                    s4: Mapping<u128, Packed>,
                    s5: Lazy<NonPacked>,
                    s6: PackedGeneric<Packed>,
                    s7: NonPackedGeneric<Packed>,
                    // Fails because: the trait `ink::storage::traits::Packed` is not implemented for `NonPacked`
                    // s8: Mapping<u128, NonPacked>,
                }
            },
        ] {
            let storage_item = parse_first_storage_item(code);

            let mut results = Vec::new();
            diagnostics(&mut results, &storage_item);
            assert!(results.is_empty(), "storage_item: {code}");
        }
    }
}
