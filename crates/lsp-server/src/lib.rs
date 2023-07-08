//! A [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) implementation for the [ink!](https://use.ink/) smart contract programming language.
//!
//! Installation and usage instructions for the ink! Language Server (`ink-lsp-server`) binary can be found in the [crate README on GitHub](https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/lsp-server) (or [crates.io](https://crates.io/crates/ink-lsp-server)).

pub use {dispatch::main_loop, initialize::initialize};

mod dispatch;
mod initialize;
mod memory;
mod test_utils;
pub mod translator;
mod utils;
