//! Types and abstractions for performing semantic analysis of ink! smart contract code.

use ink_analyzer_ir::InkFile;

pub use diagnostics::{Diagnostic, Severity};

mod diagnostics;

/// Analysis is the main entry point for asking for semantic information about ink! smart contract code.
#[derive(Debug)]
pub struct Analysis;

impl Analysis {
    /// Returns the intermediate representation (IR) of the smart contract code.
    pub fn parse(&self, code: &str) -> InkFile {
        InkFile::parse(code)
    }

    /// Runs diagnostics for the smart contract code.
    pub fn diagnostics(&self, code: &str) -> Vec<Diagnostic> {
        diagnostics::diagnostics(&self.parse(code))
    }
}
