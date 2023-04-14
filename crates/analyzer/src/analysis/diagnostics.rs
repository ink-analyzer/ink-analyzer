//! Diagnostic errors and warnings based on ink! semantic rules.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::InkFile;

mod contract;
mod file;
pub mod utils;

/// A diagnostic error or warning.
#[derive(Debug)]
pub struct Diagnostic {
    /// Error or warning message.
    pub message: String,
    /// Text range to highlight.
    pub range: TextRange,
    /// The severity level of the diagnostic.
    pub severity: Severity,
}

/// The severity level of the diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// A diagnostic error.
    Error,
    /// A diagnostic warning.
    Warning,
}

/// Computes diagnostics for the source file.
pub fn diagnostics(file: &InkFile) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // ink! file level diagnostics.
    utils::append_diagnostics(&mut results, &mut file::diagnostics(file));

    // ink! contract diagnostics.
    utils::append_diagnostics(
        &mut results,
        &mut file
            .contracts()
            .iter()
            .flat_map(contract::diagnostics)
            .collect::<Vec<Diagnostic>>(),
    );

    results
}
