//! A [Language Server Protocol (LSP)][LSP] implementation for the [ink!] smart contract programming language.
//!
//! Installation and usage instructions for the ink! Language Server (`ink-lsp-server`) binary
//! can be found in the [crate README on GitHub][GitHub] (or [crates.io][crates-io]).
//!
//! [LSP]: https://microsoft.github.io/language-server-protocol/
//! [ink!]: https://use.ink/
//! [GitHub]: https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/lsp-server
//! [crates-io]: https://crates.io/crates/ink-lsp-server

mod dispatch;
mod initialize;
mod memory;
mod test_utils;
pub mod translator;
mod utils;

pub use {dispatch::main_loop, initialize::initialize};
