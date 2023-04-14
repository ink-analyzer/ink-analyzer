# ink! Analyzer IR

[ink!](https://use.ink/) intermediate representations (IRs) and abstractions for [ink! analyzer](/crates/analyzer).

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer-ir
```

## Usage

### Example: Generate an IR of ink! smart contract code


```rust
use ink_analyzer_ir::InkFile;

fn generate_ir() {
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

    let file = InkFile::parse(code);
    dbg!(&file);
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
