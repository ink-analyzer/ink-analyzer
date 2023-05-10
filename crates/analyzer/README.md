# 🦑 ink! Analyzer

A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

It implements utilities for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of ink! analyzer at a high level.

It currently only implements a public interface that accepts a string representation (`&str`) of ink! smart contract code as input and returns a list of diagnostics (`Vec<Diagnostic>`) as output.

The diagnostic model includes:
- an error/warning message.
- the text range to which the diagnostic applies
- the severity (e.g error or warning).

You can find the low-level diagnostic implementations in the [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) module and its ink! entity specific [submodules](/crates/analyzer/src/analysis/diagnostics).

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer
```

## Usage

### Example:
Run diagnostics for ink! smart contract code.


```rust
use ink_analyzer::Analysis;

fn do_analysis() {
    let code = r#"
        #[ink::contract]
        mod my_contract {

            #[ink(storage)]
            pub struct MyContract {
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

You can run unit and integration tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-analyzer-ir
```

Implementations of the unit tests (and hence a good overview of the current functionality) can be found in the [diagnostics submodule](/crates/analyzer/src/analysis/diagnostics.rs) of the [ink-analyzer crate](/crates/analyzer).

## License

Licensed under either [MIT](/LICENSE-MIT) or [Apache-2.0](/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
