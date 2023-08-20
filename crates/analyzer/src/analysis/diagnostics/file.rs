//! ink! file level diagnostics.

use ink_analyzer_ir::{InkAttributeKind, InkFile};

use super::{
    chain_extension, contract, ink_e2e_test, ink_test, storage_item, trait_definition, utils,
};
use crate::{Diagnostic, Severity};

/// Runs ink! file level diagnostics.
pub fn diagnostics(results: &mut Vec<Diagnostic>, file: &InkFile) {
    // Runs generic diagnostics `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, file);

    // Ensures that at most one ink! contract, See `ensure_contract_quantity`.
    ensure_contract_quantity(results, file);

    // ink! contract diagnostics.
    file.contracts()
        .iter()
        .for_each(|item| contract::diagnostics(results, item));

    // Runs ink! trait definition diagnostics, see `trait_definition::diagnostics` doc.
    file.trait_definitions()
        .iter()
        .for_each(|item| trait_definition::diagnostics(results, item));

    // Runs ink! chain extension diagnostics, see `chain_extension::diagnostics` doc.
    file.chain_extensions()
        .iter()
        .for_each(|item| chain_extension::diagnostics(results, item));

    // Runs ink! storage item diagnostics, see `storage_item::diagnostics` doc.
    file.storage_items()
        .iter()
        .for_each(|item| storage_item::diagnostics(results, item));

    // Runs ink! test diagnostics, see `ink_test::diagnostics` doc.
    file.tests()
        .iter()
        .for_each(|item| ink_test::diagnostics(results, item));

    // Runs ink! e2e test diagnostics, see `ink_e2e_test::diagnostics` doc.
    file.e2e_tests()
        .iter()
        .for_each(|item| ink_e2e_test::diagnostics(results, item));

    // Ensures that only ink! attribute macro quasi-direct descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, file);
}

/// Ensures that there are not multiple ink! contract definitions.
///
/// Multiple ink! contract definitions in a single file generate conflicting metadata definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/codegen/src/generator/metadata.rs#L51>.
fn ensure_contract_quantity(results: &mut Vec<Diagnostic>, file: &InkFile) {
    utils::ensure_at_most_one_item(
        results,
        file.contracts(),
        "Only one ink! contract per file is currently supported.",
        Severity::Error,
    );
}

/// Ensures that only ink! attribute macro quasi-direct descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L98-L114>.
fn ensure_valid_quasi_direct_ink_descendants(results: &mut Vec<Diagnostic>, file: &InkFile) {
    utils::ensure_valid_quasi_direct_ink_descendants(results, file, |attr| {
        matches!(attr.kind(), InkAttributeKind::Macro(_))
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::InkFile;
    use quote::format_ident;
    use test_utils::quote_as_str;

    #[test]
    fn one_contract_definition_works() {
        let file = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }
        });

        let mut results = Vec::new();
        ensure_contract_quantity(&mut results, &file);
        assert!(results.is_empty());
    }

    #[test]
    fn no_contract_definitions_works() {
        let file = InkFile::parse("");

        let mut results = Vec::new();
        ensure_contract_quantity(&mut results, &file);
        assert!(results.is_empty());
    }

    #[test]
    fn multiple_contract_definitions_fails() {
        // Tests snippets with btn 2 and 5 contract definitions.
        for idx in 2..=5 {
            // Creates code snippets with multiple contract definitions.
            let code = (1..=idx)
                .map(|i| {
                    let name = format_ident!("my_contract{i}");
                    (quote_as_str! {
                        #[ink::contract]
                        mod #name {
                        }
                    })
                    .to_string()
                })
                .collect::<String>();

            let file = InkFile::parse(&code);

            let mut results = Vec::new();
            ensure_contract_quantity(&mut results, &file);
            // There should be `idx-1` extraneous contract definitions.
            assert_eq!(results.len(), idx - 1);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                idx - 1
            );
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        let contract = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }

            #[ink::trait_definition]
            trait MyTrait {
            }

            #[ink::chain_extension]
            trait MyChainExtension {
            }

            #[ink::storage_item]
            struct MyStorageItem {
            }

            #[cfg(test)]
            mod tests {
                #[ink::test]
                fn it_works() {

                }
            }
        });

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &contract);
        assert!(results.is_empty());
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let contract = InkFile::parse(quote_as_str! {
            #[ink(storage)]
            struct MyContract {
            }

            #[ink(event)]
            struct MyEvent {
                #[ink(topic)]
                value: bool,
            }

            impl MyContract {
                #[ink(constructor)]
                pub fn my_constructor() -> Self {
                }

                #[ink(message)]
                pub fn my_message(&mut self) {
                }
            }
        });

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &contract);
        // There should be 4 errors (i.e `storage`, `event`, `constructor` and `message`,
        // `topic` is not a quasi-direct dependant because it has `event` as a parent).
        assert_eq!(results.len(), 4);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            4
        );
    }
}
