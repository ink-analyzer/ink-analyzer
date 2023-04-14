# 🦑 ink! Analyzer

A collection of modular and reusable libraries and tools for semantic analysis of [ink!](https://use.ink/) smart contract code.

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

## Architecture

This repository contains 3 crates:

### 1. [Semantic Analyzer (ink-analyzer)](/crates/analyzer)
This crate implements types and abstractions for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of this library.

Currently only diagnostics that return a diagnostic model that includes an error/warning message, the text range to which the diagnostic applies and its severity are implemented.

You can find their core implementations in the [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) submodule.

### 2. [IR (ink-analyzer-ir)](/crates/ir)
This crate implements ink! intermediate representations (IRs) and abstractions.

### 3. [Proc-macros (ink-analyzer-macro)](/crates/macro)
This crate implements procedural macros used by other crates e.g. custom derive macros for IR traits.

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

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test
```

## License

This code is released under both MIT and Apache-2.0 licenses.
