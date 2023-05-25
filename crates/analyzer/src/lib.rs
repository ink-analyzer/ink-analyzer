//! # ink! Analyzer
//! A library for semantic analysis of [ink!](https://use.ink/) smart contract code.
//!
//! # Example
//! Run diagnostics for ink! smart contract code.
//!
//! ```
//! use ink_analyzer::Analysis;
//!
//! fn do_analysis() {
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
//!     let diagnostics = Analysis::new(code).diagnostics();
//!     dbg!(&diagnostics);
//! }
//! ```

pub use self::{
    analysis::Analysis, analysis::Completion, analysis::Diagnostic, analysis::Severity,
};

mod analysis;
