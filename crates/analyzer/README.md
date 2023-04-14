# 🦑 ink! Analyzer

A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

It implements types and abstractions for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of this library.

Currently only diagnostics that return a diagnostic model that includes an error/warning message, the text range to which the diagnostic applies and its severity are implemented.

You can find their core implementations in the [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) submodule.

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

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

    let diagnostics = Analysis.diagnostics(code);
    dbg!(&diagnostics);
}
```

## Documentation

[https://docs.rs/ink-analyzer/latest/ink_analyzer/](https://docs.rs/ink-analyzer/latest/ink_analyzer/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc -p ink-analyzer-ir --open
```

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-analyzer-ir
```

Implementations of the unit tests (and hence a good overview of the current functionality) can be found in the [diagnostics submodule](/crates/analyzer/src/analysis/diagnostics.rs) of the [ink-analyzer crate](/crates/analyzer).

## License

This code is released under both MIT and Apache-2.0 licenses.