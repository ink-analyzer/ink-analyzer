//! Diagnostic errors and warnings based on ink! semantic rules.
//!
//! # Note
//! The [ink_ir crate](https://github.com/paritytech/ink/tree/v4.1.0/crates/ink/ir) is used as a reference implementation for ink!'s semantic rules.
//!
//! References to the source of enforced semantic rules are included either in the rustdoc for most utilities or at the call site for more generic utilities.
//!
//! ## Methodology for extracting ink! semantic rules
//! 1. Start by reviewing each ink! attribute macro's definition in the [ink_macro crate](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs).
//! 2. Then for each ink! attribute macro, extract its semantic rules from its corresponding [ink_ir](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/lib.rs) type,
//! the types, utilities and modules it uses, as well as related unit tests.
//!
//! Using the [`#[ink::contract]` attribute-macro](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L517-L520) as an example,
//! from its [implementation in the ink_macro crate](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/contract.rs#L20-L30),
//! we can trace it's corresponding `ink_ir` type as the [Contract struct in the contracts module of the ink_ir crate](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs).
//!
//! We can then extract the semantic rules by recursively analyzing the types, utilities and modules
//! used in the [Contract struct's constructor](https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs#L61-L73) as well as related unit tests.

use ink_analyzer_ir::syntax::TextRange;
use ink_analyzer_ir::InkFile;
use itertools::Itertools;

use crate::{Action, TextEdit};

mod file;
mod utils;

mod chain_extension;
mod constructor;
mod contract;
mod event;
mod extension;
mod ink_e2e_test;
mod ink_impl;
mod ink_test;
mod message;
mod storage;
mod storage_item;
mod topic;
mod trait_definition;

/// A diagnostic error or warning.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// Error or warning message.
    pub message: String,
    /// Text range to highlight.
    pub range: TextRange,
    /// The severity level of the diagnostic.
    pub severity: Severity,
    /// Quickfixes (suggested edits/actions) for the diagnostic (if any).
    pub quickfixes: Option<Vec<Action>>,
}

/// The severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// A diagnostic error.
    Error,
    /// A diagnostic warning.
    Warning,
}

/// Runs diagnostics for the source file.
pub fn diagnostics(file: &InkFile) -> Vec<Diagnostic> {
    let mut results = Vec::new();
    file::diagnostics(&mut results, file);
    results
        .into_iter()
        // Deduplicate by range, severity and quickfix edits.
        .unique_by(|item| {
            let quickfix_edits: Option<Vec<TextEdit>> = item
                .quickfixes
                .as_ref()
                .map(|it| it.iter().flat_map(|it| it.edits.clone()).collect());
            (item.range, item.severity, quickfix_edits)
        })
        .collect()
}
