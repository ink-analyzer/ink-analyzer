//! # ink! Analyzer
//! A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

pub use self::{analysis::Analysis, analysis::Diagnostic, analysis::Severity};

// Re-export ink_analyzer_ir as ir.
pub use ink_analyzer_ir as ir;

mod analysis;
