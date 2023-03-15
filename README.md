# ink! Analyzer

A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

**NOTE:** This project is still work in progress and implements very limited functionality at this time.

## Quick Start

This project currently contains 2 main modules:

### 1. [analysis](./src/analysis/mod.rs)
This module implements types and abstractions for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of this library.

Currently only a diagnostic that detects when the `#[ink::contract]` attribute is applied to anything
other than a `mod` item and returns a diagnostic model that includes an error message
and the text range to which the diagnostic applies is implemented.
You can find its core implementation in the [diagnostics](./src/analysis/diagnostics.rs) submodule.

### 2. [ir](./src/ir/mod.rs)
This module implements ink! intermediate representations (IRs) and abstractions.

## Documentation

You can access documentation for this project locally by running the following command from the project root

```shell
cargo doc --open
```

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test
```

Implementations of the unit tests (and hence a good overview of the current functionality) can be found in the [analysis module](./src/analysis/mod.rs).

## License

This code is released under both MIT and Apache-2.0 licenses.
