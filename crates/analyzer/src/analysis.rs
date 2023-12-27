//! Types and abstractions for performing semantic analysis of ink! smart contract code.

mod actions;
mod completions;
mod diagnostics;
mod hover;
mod inlay_hints;
mod signature_help;
mod text_edit;
mod utils;

use ink_analyzer_ir::syntax::{TextRange, TextSize};
use ink_analyzer_ir::InkFile;
use itertools::Itertools;

pub use actions::{Action, ActionKind};
pub use completions::Completion;
pub use diagnostics::{Diagnostic, Severity};
pub use hover::Hover;
pub use inlay_hints::InlayHint;
pub use signature_help::SignatureHelp;
pub use text_edit::TextEdit;

/// Entry point for asking for semantic information about ink! smart contract code.
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

    /// Computes ink! attribute completions at the given position.
    pub fn completions(&self, position: TextSize) -> Vec<Completion> {
        completions::completions(&self.file, position)
    }

    /// Computes ink! attribute code/intent actions for the given text range.
    pub fn actions(&self, range: TextRange) -> Vec<Action> {
        // Returns quickfixes (for diagnostics) + generic code actions.
        diagnostics::diagnostics(&self.file)
            .into_iter()
            .filter_map(|it| it.quickfixes)
            .flatten()
            // Filters out diagnostics that apply to the given text range.
            .filter(|action| {
                range.contains_range(action.range) || action.range.contains_range(range)
            })
            // Combines quickfixes and generic actions (with quickfixes taking priority).
            .chain(actions::actions(&self.file, range))
            // Deduplicate by edits.
            .unique_by(|item| item.edits.clone())
            .collect()
    }

    /// Returns descriptive/informational text for the ink! attribute at the given text range (if any).
    pub fn hover(&self, range: TextRange) -> Option<Hover> {
        hover::hover(&self.file, range)
    }

    /// Computes ink! attribute argument inlay hints for the given text range (if any).
    pub fn inlay_hints(&self, range: Option<TextRange>) -> Vec<InlayHint> {
        inlay_hints::inlay_hints(&self.file, range)
    }

    /// Computes ink! attribute signature help for the given position.
    pub fn signature_help(&self, position: TextSize) -> Vec<SignatureHelp> {
        signature_help::signature_help(&self.file, position)
    }
}
