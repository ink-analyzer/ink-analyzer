# ink! analyzer IR

[ink!] intermediate representations (IRs) and abstractions for [ink! analyzer].

This library implements types and abstractions for all ink! entities (e.g. contracts, storage, events, 
topics, impls, constructors, messages, selectors, tests, trait definitions, chain extensions, storage items e.t.c).

It uses [rust-analyzer]'s [ra_ap_syntax] crate for generating the syntax tree of the ink! smart contract code that it then converts into ink! entity intermediate representations and abstractions.

The [ra_ap_syntax] crate is used (instead of other Rust parsing and syntax tree libraries) 
because ink! analyzer has similar [design goals][goals] to rust-analyzer.
The most important being that parsing should be:
- resilient (even if the input is invalid, parser tries to see as much syntax tree fragments in the input as it can).
- lossless (even if the input is invalid, the tree produced by the parser represents it exactly).

It's the main dependency for the [semantic analyzer][ink! analyzer] crate.

[ink!]: https://use.ink/
[ink! analyzer]: https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/analyzer
[rust-analyzer]: https://github.com/rust-lang/rust-analyzer
[ra_ap_syntax]: https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/
[goals]: https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md#design-goals

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer-ir
```

## Usage

### Example:
Generate an IR of ink! smart contract code.

```rust
use ink_analyzer_ir::InkFile;

fn generate_ir() {
    let file = InkFile::parse(r#"
        #[ink::contract]
        mod my_contract {

            #[ink(storage)]
            pub struct MyContract {
                value: bool,
            }

            // --snip--
        }
    "#);
    dbg!(&file);

    let contracts = file.contracts();
    dbg!(&contracts);

    if let Some(contract) = contracts.first() {
        let storage = contract.storage();
        dbg!(&storage);
    }
}
```

## Documentation

<https://docs.rs/ink-analyzer-ir/latest/ink_analyzer_ir/>

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

Licensed under either [MIT] or [Apache-2.0] license at your option.

[MIT]: https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-MIT
[Apache-2.0]: https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-APACHE

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
