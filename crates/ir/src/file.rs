//! ink! source file IR.

use ink_analyzer_macro::FromAST;
use ra_ap_syntax::{AstNode, SourceFile};

use crate::{utils, Contract, FromAST, FromInkAttribute, InkAttributeKind, InkMacroKind};

/// An ink! source file.
#[derive(Debug, Clone, PartialEq, Eq, FromAST)]
pub struct InkFile {
    /// List of top level (i.e not nested) ink! contracts in source file.
    contracts: Vec<Contract>,
    /// AST Node for ink! source file.
    ast: SourceFile,
}

impl From<SourceFile> for InkFile {
    fn from(file: SourceFile) -> Self {
        let mut contracts = Vec::new();
        let ink_descendants = utils::ink_attrs_closest_descendants(file.syntax());
        for item in ink_descendants {
            if let InkAttributeKind::Macro(InkMacroKind::Contract) = item.kind() {
                contracts.push(Contract::cast(item).expect("Should be able to cast contract"))
            }
        }
        Self {
            contracts,
            ast: file,
        }
    }
}

impl InkFile {
    /// Parses ink! IR from source code.
    pub fn parse(code: &str) -> Self {
        Self::from(SourceFile::parse(code).tree())
    }

    /// Returns list of top level (i.e not nested) ink! contracts in source file.
    pub fn contracts(&self) -> &[Contract] {
        &self.contracts
    }
}
