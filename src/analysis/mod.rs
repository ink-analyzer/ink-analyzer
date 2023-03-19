//! Types and abstractions for performing semantic analysis of ink! smart contract code.
use ra_ap_syntax::SourceFile;

pub use crate::analysis::diagnostics::{Diagnostic, Severity};

mod diagnostics;

/// Analysis is the main entry point for asking for semantic information about source code.
#[derive(Debug)]
pub struct Analysis;

impl Analysis {
    /// Gets the syntax tree of the smart contract code.
    pub fn parse(&self, code: &str) -> SourceFile {
        SourceFile::parse(code).tree()
    }

    /// Computes diagnostics for the smart contract code.
    pub fn diagnostics(&self, code: &str) -> Vec<Diagnostic> {
        diagnostics::diagnostics(&self.parse(code))
    }
}
