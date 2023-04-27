//! Diagnostic errors and warnings based on ink! semantic rules.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::InkFile;

mod file;
mod utils;

mod chain_extension;
mod constructor;
mod contract;
mod event;
mod extension;
mod ink_impl;
mod ink_test;
mod message;
mod storage;
mod storage_item;
mod topic;
mod trait_definition;

/// A diagnostic error or warning.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic {
    /// Error or warning message.
    pub message: String,
    /// Text range to highlight.
    pub range: TextRange,
    /// The severity level of the diagnostic.
    pub severity: Severity,
}

/// The severity level of the diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// A diagnostic error.
    Error,
    /// A diagnostic warning.
    Warning,
}

/// Runs diagnostics for the source file.
pub fn diagnostics(file: &InkFile) -> Vec<Diagnostic> {
    file::diagnostics(file)
}
