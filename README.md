# 🦑 ink! Analyzer

A collection of modular and reusable libraries and tools for semantic analysis of [ink!](https://use.ink/) smart contract code.

ink! analyzer aims to improve [ink!](https://use.ink/) language support in [integrated development environments (IDEs)](https://en.wikipedia.org/wiki/Integrated_development_environment), [source code editors](https://en.wikipedia.org/wiki/Source-code_editor) and other development tools by providing modular and reusable building blocks for implementing features like diagnostics, code completion, code/intent actions and hover content for the [ink! programming language](https://use.ink/) which is used for writing smart contracts for blockchains built on [Substrate](https://substrate.io/).

**NOTE:** 🚧 This project is still work in progress, check back over the next few weeks for regular updates.

## Architecture

This repository contains 3 crates:

### 1. [Semantic Analyzer (ink-analyzer)](/crates/analyzer)
This crate implements utilities for performing semantic analysis of ink! smart contract code.
It therefore implements the core functionality of ink! analyzer at a high level.

It currently implements an [Analysis](/crates/analyzer/src/analysis.rs) entry point that accepts a string representation (`&str`) of ink! smart contract code as input and defines associated methods that compute:

- [diagnostics](/crates/analyzer/src/analysis/diagnostics.rs) - errors and warnings based on ink! semantic rules.
- [completions](/crates/analyzer/src/analysis/completions.rs) - completion suggestions for ink! attribute macros and arguments.
- [code/intent actions](/crates/analyzer/src/analysis/actions.rs) - contextual assists for adding relevant ink! attribute macros and arguments.
- [hover content](/crates/analyzer/src/analysis/hover.rs) - descriptive/informational text for ink! attribute macros and arguments.

### 2. [IR (ink-analyzer-ir)](/crates/ir)
This crate implements types, abstractions and utilities for parsing ink! smart contract code into ink! intermediate representations (IRs) and abstractions.

It implements types and abstractions for all ink! entities (e.g contracts, storage, events, topics, impls, constructors, messages, selectors, tests, trait definitions, chain extensions, storage items e.t.c).

It uses [rust-analyzer](https://github.com/rust-lang/rust-analyzer)'s [ra_ap_syntax](https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/) crate for generating the syntax tree
of the ink! smart contract code that it then converts into ink! entity intermediate representations and abstractions.

It uses [ra_ap_syntax](https://docs.rs/ra_ap_syntax/latest/ra_ap_syntax/) instead of other Rust parsing and syntax tree libraries because ink! analyzer has similar [design goals](https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/syntax.md#design-goals) to rust-analyzer.
The most important being that parsing should be:
- resilient (even if the input is invalid, parser tries to see as much syntax tree fragments in the input as it can).
- lossless (even if the input is invalid, the tree produced by the parser represents it exactly).

It's the main dependency for the [semantic analyzer](/crates/analyzer) crate.

### 3. [Proc-macros (ink-analyzer-macro)](/crates/macro)
This crate implements procedural macros used primarily by the [ink-analyzer-ir](/crates/ir) crate (e.g. custom derive macros for ink! entity traits).

## Installation and Usage

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

To open crate specific docs, see instructions in the readme in each crate's directory.

## Testing

You can run unit and integration tests for all the core functionality for all crates by running the following command from the project root

### Option 1: Native Rust and cargo

```shell
cargo test
```

**NOTE:** To run only tests for a single crate, add a `-p <crate_name>` argument to the above command e.g.
```shell
cargo test -p ink-analyzer-ir
```

### Option 2: Docker

Build the docker image.
```shell
docker build -t ink-analyzer .
```

Run tests from the container.
```shell
docker run -it ink-analyzer
```

**NOTE:** To run only tests for a single crate, add a `-p <crate_name>` argument to the docker run command e.g.
```shell
docker run -it ink-analyzer -p ink-analyzer-ir
```

## License

Licensed under either [MIT](/LICENSE-MIT) or [Apache-2.0](/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

## Acknowledgements

🌱 Funded by: the [Web3 Foundation](https://web3.foundation/).
