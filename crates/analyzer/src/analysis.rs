//! Types and abstractions for performing semantic analysis of ink! smart contract code.

use ink_analyzer_ir::syntax::TextSize;
use ink_analyzer_ir::InkFile;

pub use completions::Completion;
pub use diagnostics::{Diagnostic, Severity};

mod completions;
mod diagnostics;
mod utils;

/// Analysis is the main entry point for asking for semantic information about ink! smart contract code.
#[derive(Debug)]
pub struct Analysis {
    /// The ink! smart contract code being analyzed.
    file: InkFile,
}

impl Analysis {
    /// Creates an analysis instance from smart contract code.
    pub fn new(code: &str) -> Self {
        Self {
            file: InkFile::parse(code),
        }
    }

    /// Returns the intermediate representation (IR) of the smart contract code.
    pub fn file(&self) -> &InkFile {
        &self.file
    }

    /// Runs diagnostics for the smart contract code.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        diagnostics::diagnostics(&self.file)
    }

    /// Computes completions at the given position.
    pub fn completions(&self, position: TextSize) -> Vec<Completion> {
        completions::completions(&self.file, position)
    }
}
