# ink! Analyzer IR

[ink!](https://use.ink/) intermediate representations (IRs) and abstractions for [ink! analyzer](/crates/analyzer).

This library implements types and abstractions for all ink! entities (e.g contracts, storage, events, topics, impls, constructors, messages, selectors, tests, trait definitions, chain extensions, storage items e.t.c).

It uses [rust-analyzer](https://github.com/rust-lang/rust-analyzer)'s [ra_ap_syntax](https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/) crate for generating the syntax tree
of the ink! smart contract code that it then converts into ink! entity intermediate representations and abstractions.

It uses [ra_ap_syntax](https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/) instead of other Rust parsing and syntax tree libraries because ink! analyzer has similar [design goals](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md#design-goals) to rust-analyzer.
The most important being that parsing should be:
- resilient (even if the input is invalid, parser tries to see as much syntax tree fragments in the input as it can).
- lossless (even if the input is invalid, the tree produced by the parser represents it exactly).

It's the main dependency for the [semantic analyzer](/crates/analyzer) crate.

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer-ir
```

## Usage

### Example:
Generate an IR of ink! smart contract code.

```rust
use ink_analyzer_ir::{InkFile, quote_as_str};

fn generate_ir() {
    let file = InkFile::parse(quote_as_str! {
        #[ink::contract]
        mod my_contract {

            #[ink(storage)]
            pub struct MyContract {
                value: bool,
            }

            #[ink(event)]
            pub struct MyEvent {
                #[ink(topic)]
                value: bool,
            }

            // --snip--
        }
    });
    dbg!(&file);

    let contracts = file.contracts();
    dbg!(&contracts);

    if let Some(contract) = contracts.first() {
        let events = contract.events();
        dbg!(&events);
    }
}
```

## Documentation

[https://docs.rs/ink-analyzer-ir/latest/ink_analyzer_ir/](https://docs.rs/ink-analyzer-ir/latest/ink_analyzer_ir/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc -p ink-analyzer-ir --open
```

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-analyzer-ir
```

## License

Licensed under either [MIT](/LICENSE-MIT) or [Apache-2.0](/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
