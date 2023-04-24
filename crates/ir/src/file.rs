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
            contracts: utils::ink_closest_descendants(file.syntax()),
            trait_definitions: utils::ink_closest_descendants(file.syntax()),
            chain_extensions: utils::ink_closest_descendants(file.syntax()),
            storage_items: utils::ink_closest_descendants(file.syntax()),
            tests: utils::ink_closest_descendants(file.syntax()),
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
