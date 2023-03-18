# ink! Analyzer

A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

**NOTE:** This project is still work in progress and implements very limited functionality at this time.

## Architecture

This project currently contains 2 main modules:

### 1. [analysis](./src/analysis/mod.rs)
This module implements types and abstractions for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of this library.

Currently only 3 diagnostics that return a diagnostic model that includes an error/warning message, the text range to which the diagnostic applies and its severity are implemented:
1. an error diagnostic that detects when the `#[ink::contract]` attribute is applied to anything other than a `mod` item.
2. a warning diagnostic for unknown ink! path-based attributes (e.g. `#[ink::xyz]`, `#[ink::abc::xyz]`).
3. a warning diagnostic for unknown ink! argument-based attributes (e.g. `#[ink(xyz)]`).

You can find their core implementations in the [diagnostics](./src/analysis/diagnostics.rs) submodule.

### 2. [ir](./src/ir/mod.rs)
This module implements ink! intermediate representations (IRs) and abstractions.

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer
```

## Usage

### Example: Get diagnostics for ink! smart contract code


```rust
use ink_analyzer::Analysis;

fn do_analysis() {
    let code = r#"
        #[ink::contract]
        mod flipper {

            #[ink(storage)]
            pub struct Flipper {
                value: bool,
            }

            // --snip--
        }
    "#;

    let diagnostics = Analysis.diagnostics(&code);
    dbg!(&diagnostics);
}
```

## Documentation

[https://docs.rs/ink-analyzer/latest/ink_analyzer/](https://docs.rs/ink-analyzer/latest/ink_analyzer/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc --open
```

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test
```

Implementations of the unit tests (and hence a good overview of the current functionality) can be found in the [diagnostics submodule](./src/analysis/diagnostics.rs) of the [analysis module](./src/analysis/mod.rs).

## License

This code is released under both MIT and Apache-2.0 licenses.
