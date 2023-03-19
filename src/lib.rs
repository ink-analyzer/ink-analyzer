//! # ink! Analyzer
//! A library for semantic analysis of [ink!](https://use.ink/) smart contract code.
//!
//! **NOTE:** This project is still work in progress
//! and implements very limited functionality at this time.

pub use crate::analysis::Analysis;

pub mod analysis;
mod ir;
