# ink! Language Server

A Language Server Protocol (LSP) implementation for the [ink!](https://use.ink/) smart contract programming language.

It implements the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) and acts as a backend that provides language support features like diagnostic errors, code completion suggestions, code/intent actions and hover content to IDEs, code editors and other development tools.

It uses the [semantic analyzer](/crates/analyzer) as the engine for providing ink! language support features by:
- translating LSP requests into semantic analyzer interface calls.
- translating semantic analysis results into corresponding LSP types.

It additionally uses rust-analyzer's [lsp-server](https://docs.rs/lsp-server/latest/lsp_server/) crate to handle LSP protocol handshaking and parsing messages and the [lsp-types](https://docs.rs/lsp-types/latest/lsp_types/) crate for LSP type definitions.

It can be reused by multiple IDEs, code editors and other development tools that support LSP servers including [Visual Studio Code, Visual Studio, Vim / Neovim, Emacs, Atom, Sublime Text, Acme, Lapce, Eclipse and many more](https://microsoft.github.io/language-server-protocol/implementors/tools/).

**NOTE:** 🚧 This project is still work in progress, check back over the next few weeks for regular updates.

## Installation

Run the following Cargo command in your project directory

```shell
cargo install ink-lsp-server
```

## Usage

### Example:
Making an LSP request.

```shell

```

## Documentation

[https://docs.rs/ink-lsp-server/latest/ink_lsp_server/](https://docs.rs/ink-lsp-server/latest/ink_lsp_server/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc -p ink-lsp-server --open
```

## Testing

You can run unit and integration tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-lsp-server
```

## License

Licensed under either [MIT](/LICENSE-MIT) or [Apache-2.0](/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
