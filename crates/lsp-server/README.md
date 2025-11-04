# <img src="https://raw.githubusercontent.com/ink-analyzer/ink-analyzer/master/images/squink.svg" width="32px" height="32px" style="margin-bottom: -3px"> ink! Language Server

A [Language Server Protocol (LSP)][LSP] implementation for the [ink!] smart contract programming language.

It implements the [Language Server Protocol (LSP)][LSP] and acts as a backend that provides language support features 
like diagnostic errors, code completion suggestions, code/intent actions, inlay hints, signature help and hover content 
to IDEs, code editors and other development tools.

It uses the [semantic analyzer][analyzer] as the engine for providing ink! language support features by:
- translating LSP requests into semantic analyzer interface calls.
- translating semantic analysis results into corresponding LSP types.

It additionally uses [rust-analyzer]'s [lsp-server][ra-lsp-server] crate 
to handle LSP protocol handshaking and parsing messages, and the [lsp-types] crate for LSP type definitions.

[LSP]: https://microsoft.github.io/language-server-protocol/ 
[ink!]: https://use.ink/
[analyzer]: https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/analyzer
[rust-analyzer]: https://github.com/rust-lang/rust-analyzer
[ra-lsp-server]: https://docs.rs/lsp-server/latest/lsp_server/
[lsp-types]: https://docs.rs/lsp-types/latest/lsp_types/

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

Copy the compiled binary (named `ink-lsp-server`) from the `target/release` directory 
to your preferred installation location, make sure the binary is executable 
and the installation location is included in the `PATH` environment variable.

## Usage

The installed ink! Language Server binary can be used with any [LSP client][LSP-client] 
that can be configured to launch an LSP server using an executable command 
(i.e. the path to the `ink-lsp-server` binary) and can use stdio (standard in/standard out) as the message transport.

While a few editors/IDEs have native/built-in LSP clients (e.g. [Neovim]), 
[most LSP clients are plugins/extensions for editors/IDEs][LSP-client].

[LSP-client]: https://microsoft.github.io/language-server-protocol/implementors/tools/
[Neovim]: https://neovim.io/doc/user/lsp.html

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

<https://docs.rs/ink-lsp-server/latest/ink_lsp_server/>

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

Licensed under either [MIT] or [Apache-2.0] license at your option.

[MIT]: https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-MIT
[Apache-2.0]: https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-APACHE

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
