//! # ink! Analyzer
//! A library for semantic analysis of [ink!](https://use.ink/) smart contract code.
//!
//! # Example
//! Analyzing ink! smart contract code.
//!
//! ```
//! use ink_analyzer::{Analysis, TextSize, TextRange};
//!
//! fn do_analysis() {
//!     // Smart contract code.
//!     let code = r#"
//!         #[ink::contract]
//!         mod my_contract {
//!
//!             #[ink(storage)]
//!             pub struct MyContract {
//!                 value: bool,
//!             }
//!
//!             // --snip--
//!         }
//!     "#;
//!
//!     // Creates analysis snapshot.
//!     let analysis = Analysis::new(code);
//!
//!     // Computes diagnostics.
//!     let diagnostics = analysis.diagnostics();
//!     dbg!(&diagnostics);
//!
//!     // Sets the cursor position.
//!     let position = TextSize::from(9);
//!
//!     // Computes completions.
//!     let completions = analysis.completions(position);
//!     dbg!(&completions);
//!
//!     // Sets the focus range.
//!     let range = TextRange::new(position, TextSize::from(25));
//!
//!     // Computes code/intent actions.
//!     let actions = analysis.actions(range);
//!     dbg!(&actions);
//!
//!     // Gets hover content.
//!     let hover = analysis.hover(range);
//!     dbg!(&hover);
//!
//!     // Computes inlay hints.
//!     let inlay_hints = analysis.inlay_hints(None);
//!     dbg!(&inlay_hints);
//! }
//! ```

pub use self::analysis::{
    Action, ActionKind, Analysis, Completion, Diagnostic, Hover, InlayHint, Severity, TextEdit,
};
pub use ink_analyzer_ir::syntax::{TextRange, TextSize};

mod analysis;

mod test_utils;
