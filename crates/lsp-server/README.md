# ink! Language Server

A [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) implementation for the [ink!](https://use.ink/) smart contract programming language.

It implements the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) and acts as a backend that provides language support features like diagnostic errors, code completion suggestions, code/intent actions and hover content to IDEs, code editors and other development tools.

It uses the [semantic analyzer](https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/analyzer) as the engine for providing ink! language support features by:
- translating LSP requests into semantic analyzer interface calls.
- translating semantic analysis results into corresponding LSP types.

It additionally uses rust-analyzer's [lsp-server](https://docs.rs/lsp-server/latest/lsp_server/) crate to handle LSP protocol handshaking and parsing messages, and the [lsp-types](https://docs.rs/lsp-types/latest/lsp_types/) crate for LSP type definitions.

**NOTE:** 🚧 This project is still work in progress, check back over the next few weeks for regular updates.

## Installation

### Option 1: Cargo (via [crates.io](https://crates.io/crates/ink-lsp-server))

Run

```shell
cargo install ink-lsp-server
```

### Option 2: Building from source

Run
```shell
git clone https://github.com/ink-analyzer/ink-analyzer.git
cd ink-analyzer
cargo build --bin ink-lsp-server --release
```

Copy the compiled binary (named `ink-lsp-server`) from the `target/release` directory to your preferred installation location, make sure the binary is executable and the installation location is included in the `PATH` environment variable.

## Usage

The installed ink! Language Server binary can be used with any [LSP client](https://microsoft.github.io/language-server-protocol/implementors/tools/) that can be configured to launch an LSP server using an executable command (i.e. the path to the `ink-lsp-server` binary) and can use stdio (standard in/standard out) as the message transport.

While a few editors/IDEs have native/built-in LSP clients (e.g. [Neovim](https://neovim.io/doc/user/lsp.html)), [most LSP clients are plugins/extensions for editors/IDEs](https://microsoft.github.io/language-server-protocol/implementors/tools/).

## Documentation

### Binary Documentation

`ink-lsp-server` binary help text.
```console
Language Server Protocol (LSP) implementation for the ink! smart contract programming language.

Usage: ink-lsp-server

Options:
  -h, --help     Print help
  -V, --version  Print version
```

### Library Documentation

[https://docs.rs/ink-lsp-server/latest/ink_lsp_server/](https://docs.rs/ink-lsp-server/latest/ink_lsp_server/)

Or you can access the library documentation locally by running the following command from the project root

```shell
cargo doc -p ink-lsp-server --open
```

## Testing

You can run unit and integration tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-lsp-server
```

## License

Licensed under either [MIT](https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-MIT) or [Apache-2.0](https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
