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
//!     // Computes code/intent actions.
//!     let actions = analysis.actions(position);
//!     dbg!(&actions);
//!
//!     // Sets the focus range.
//!     let range = TextRange::new(position, TextSize::from(25));
//!
//!     // Gets hover content.
//!     let hover = analysis.hover(range);
//!     dbg!(&hover);
//! }
//! ```

pub use self::analysis::{Action, Analysis, Completion, Diagnostic, Hover, Severity};
pub use ink_analyzer_ir::syntax::{TextRange, TextSize};

mod analysis;
