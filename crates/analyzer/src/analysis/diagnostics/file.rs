//! ink! file level diagnostics.

use ink_analyzer_ir::{InkAttributeKind, InkFile};

use super::{chain_extension, contract, ink_test, storage_item, trait_definition, utils};
use crate::{Diagnostic, Severity};

/// Runs ink! file level diagnostics.
pub fn diagnostics(file: &InkFile) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Runs generic diagnostics `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(file));

    // Ensures that at most one ink! contract, See `ensure_contract_quantity`.
    utils::append_diagnostics(&mut results, &mut ensure_contract_quantity(file));

    // ink! contract diagnostics.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .contracts()
            .iter()
            .flat_map(contract::diagnostics)
            .collect(),
    );

    // Run ink! trait definition diagnostics, see `trait_definition::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .trait_definitions()
            .iter()
            .flat_map(trait_definition::diagnostics)
            .collect(),
    );

    // Run ink! storage item diagnostics, see `storage_item::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .storage_items()
            .iter()
            .flat_map(storage_item::diagnostics)
            .collect(),
    );

    // Run ink! test diagnostics, see `ink_test::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .tests()
            .iter()
            .flat_map(ink_test::diagnostics)
            .collect(),
    );

    // Run ink! chain extension diagnostics, see `chain_extension::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .chain_extensions()
            .iter()
            .flat_map(chain_extension::diagnostics)
            .collect(),
    );

    // Ensures that only ink! attribute macro quasi-direct descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_valid_quasi_direct_ink_descendants(file),
    );

    results
}

/// Ensures that there are not multiple ink! contract definitions.
///
/// Multiple ink! contract definitions in a single file generate conflicting metadata definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/codegen/src/generator/metadata.rs#L51>.
fn ensure_contract_quantity(file: &InkFile) -> Vec<Diagnostic> {
    utils::ensure_at_most_one_item(
        file.contracts(),
        "Only one ink! contract per file is currently supported.",
        Severity::Error,
    )
}

/// Ensures that only ink! attribute macro quasi-direct descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L98-L114>.
fn ensure_valid_quasi_direct_ink_descendants(file: &InkFile) -> Vec<Diagnostic> {
    utils::ensure_valid_quasi_direct_ink_descendants(file, |attr| {
        matches!(attr.kind(), InkAttributeKind::Macro(_))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, InkFile};
    use quote::format_ident;

    #[test]
    fn one_contract_definition_works() {
        let file = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }
        });

        let results = ensure_contract_quantity(&file);
        assert!(results.is_empty());
    }

    #[test]
    fn no_contract_definitions_works() {
        let file = InkFile::parse("");

        let results = ensure_contract_quantity(&file);
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
                .collect::<Vec<String>>()
                .join("");

            let file = InkFile::parse(&code);

            let results = ensure_contract_quantity(&file);
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

        let results = ensure_valid_quasi_direct_ink_descendants(&contract);
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

        let results = ensure_valid_quasi_direct_ink_descendants(&contract);
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
