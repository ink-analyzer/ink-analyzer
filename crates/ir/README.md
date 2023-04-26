# ink! Analyzer IR

[ink!](https://use.ink/) intermediate representations (IRs) and abstractions for [ink! analyzer](/crates/analyzer).

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

This code is released under both MIT and Apache-2.0 licenses.
