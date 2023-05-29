//! ink! source file IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::{AstNode, SourceFile};

use crate::{utils, ChainExtension, Contract, FromAST, InkTest, StorageItem, TraitDefinition};

/// An ink! source file.
#[derive(Debug, Clone, PartialEq, Eq, FromAST)]
pub struct InkFile {
    /// ink! contracts in source file.
    contracts: Vec<Contract>,
    /// ink! trait definitions in source file.
    trait_definitions: Vec<TraitDefinition>,
    /// ink! chain extensions in source file.
    chain_extensions: Vec<ChainExtension>,
    /// ink! storage items in source file.
    storage_items: Vec<StorageItem>,
    /// ink! tests in source file.
    tests: Vec<InkTest>,
    /// AST Node for ink! source file.
    ast: SourceFile,
}

impl From<SourceFile> for InkFile {
    fn from(file: SourceFile) -> Self {
        Self {
            contracts: utils::ink_closest_descendants(file.syntax()).collect(),
            trait_definitions: utils::ink_contract_wrappable_quasi_closest_descendants(
                file.syntax(),
            )
            .collect(),
            chain_extensions: utils::ink_contract_wrappable_quasi_closest_descendants(
                file.syntax(),
            )
            .collect(),
            storage_items: utils::ink_contract_wrappable_quasi_closest_descendants(file.syntax())
                .collect(),
            tests: utils::ink_closest_descendants(file.syntax()).collect(),
            ast: file,
        }
    }
}

impl InkFile {
    /// Parses ink! IR from source code.
    pub fn parse(code: &str) -> Self {
        Self::from(SourceFile::parse(code).tree())
    }

    /// Returns ink! contracts in source file.
    pub fn contracts(&self) -> &[Contract] {
        &self.contracts
    }

    /// Returns ink! trait definitions in source file.
    pub fn trait_definitions(&self) -> &[TraitDefinition] {
        &self.trait_definitions
    }

    /// Returns ink! chain extensions in source file.
    pub fn chain_extensions(&self) -> &[ChainExtension] {
        &self.chain_extensions
    }

    /// Returns ink! storage items in source file.
    pub fn storage_items(&self) -> &[StorageItem] {
        &self.storage_items
    }

    /// Returns ink! tests in source file.
    pub fn tests(&self) -> &[InkTest] {
        &self.tests
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_utils::quote_as_str;

    #[test]
    fn parse_works() {
        let file = InkFile::parse(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }

            #[ink::trait_definition]
            pub trait MyTrait {
            }

            #[ink::chain_extension]
            pub trait MyChainExtension {
            }

            #[ink::storage_item]
            struct MyStorageItem {
            }

            #[ink::storage_item]
            struct MyStorageItem2 {
            }

            #[cfg(test)]
            mod tests {
                #[ink::test]
                fn it_works {
                }

                #[ink::test]
                fn it_works2 {
                }
            }
        });

        // 1 contract.
        assert_eq!(file.contracts().len(), 1);

        // 1 trait definition.
        assert_eq!(file.trait_definitions().len(), 1);

        // 1 chain extension.
        assert_eq!(file.chain_extensions().len(), 1);

        // 2 storage items.
        assert_eq!(file.storage_items().len(), 2);

        // 2 tests.
        assert_eq!(file.tests().len(), 2);
    }
}
