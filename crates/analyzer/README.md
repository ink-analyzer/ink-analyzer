# 🦑 ink! Analyzer

A library for semantic analysis of [ink!](https://use.ink/) smart contract code.

It implements utilities for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of ink! analyzer at a high level.

It currently implements an [Analysis](/crates/analyzer/src/analysis.rs) entry point that accepts a string representation (`&str`) of ink! smart contract code as input and defines associated methods that compute:

- [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) - errors and warnings based on ink! semantic rules.
- [completions](/crates/analyzer/src/analysis/completions.rs) - completion suggestions for ink! attribute macros and arguments.
- [code/intent actions](/crates/analyzer/src/analysis/actions.rs) - contextual assists for adding relevant ink! attribute macros and arguments.
- [hover content](/crates/analyzer/src/analysis/hover.rs) - descriptive/informational text for ink! attribute macros and arguments.

**NOTE:** 🚧 This project is still work in progress, check back over the next few weeks for regular updates.

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

    let diagnostics = Analysis::new(code).diagnostics();
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
