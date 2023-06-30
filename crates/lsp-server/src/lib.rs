//! A [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) implementation for the [ink!](https://use.ink/) smart contract programming language.

pub use {dispatch::main_loop, initialize::initialize};

mod dispatch;
mod initialize;
mod memory;
mod test_utils;
pub mod translator;
