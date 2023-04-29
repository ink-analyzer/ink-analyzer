# 🦑 ink! Analyzer

A collection of modular and reusable libraries and tools for semantic analysis of [ink!](https://use.ink/) smart contract code.

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

## Architecture

This repository contains 3 crates:

### 1. [Semantic Analyzer (ink-analyzer)](/crates/analyzer)
This crate implements utilities for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of ink! analyzer at a high level.

It currently only implements diagnostics that return a diagnostic model that includes:
- an error/warning message.
- the text range to which the diagnostic applies
- the severity (e.g error or warning).

You can find their core implementations in the [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) module
and its ink! entity specific [submodules](/crates/analyzer/src/analysis/diagnostics).

### 2. [IR (ink-analyzer-ir)](/crates/ir)
This crate implements types, abstractions and utilities for parsing ink! smart contract code into ink! intermediate representations (IRs) and abstractions.

It uses rust-analyzer's [ra_ap_syntax](https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/) crate for generating the syntax tree
of the ink! smart contract code that it then converts into ink! entity intermediate representations and abstractions.

It's the main dependency for the [semantic analyzer](/crates/analyzer) crate.

### 3. [Proc-macros (ink-analyzer-macro)](/crates/macro)
This crate implements procedural macros (e.g. custom derive macros for ink! IR traits) used primarily by the [ir](/crates/ir) crate.

## Installation and Usage Instructions

Check the readme of each crate for installation and usage instructions and links to documentation.

- Analyzer: [/crates/analyzer](/crates/analyzer)
- IR: [/crates/ir](/crates/ir)
- Proc-macros: [/crates/macro](/crates/macro)

## Documentation

- Analyzer: [https://docs.rs/ink-analyzer/latest/ink_analyzer/](https://docs.rs/ink-analyzer/latest/ink_analyzer/)
- IR: [https://docs.rs/ink-analyzer-ir/latest/ink_analyzer_ir/](https://docs.rs/ink-analyzer-ir/latest/ink_analyzer_ir/)
- Proc-macros: [https://docs.rs/ink-analyzer-macro/latest/ink_analyzer_macro/](https://docs.rs/ink-analyzer-macro/latest/ink_analyzer_macro/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc --open
```

To open crate specific docs, see instructions in the readme in each crate's directory.

## Testing

You can run unit tests for all the core functionality for all crates by running the following command from the project root

```shell
cargo test
```

To run only crate specific tests, see instructions in the readme in each crate's directory.

## License

This code is released under both MIT and Apache-2.0 licenses.
