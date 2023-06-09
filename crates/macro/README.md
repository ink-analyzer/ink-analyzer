# ink! Analyzer Proc-macros

Procedural macros for [ink-analyzer](https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/analyzer) and [ink-analyzer-ir](https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/ir).

This library implements procedural macros used primarily by the [ink-analyzer-ir](https://github.com/ink-analyzer/ink-analyzer/tree/master/crates/ir) crate (e.g. custom derive macros for ink! entity traits).

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer-macro
```

## Usage

### Example:
Using custom derive macros for the `FromInkAttribute` and `FromSyntax` traits to create a `Contract` type.

```rust
use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
use ink_analyzer_ir::{Event, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message, Storage};
use ink_analyzer_ir::ast;

#[derive(FromInkAttribute, FromSyntax)]
struct Contract {
    #[macro_kind(Contract)]
    ink_attr: InkAttrData<ast::Module>,
    #[arg_kind(Storage)]
    storage: Option<Storage>,
    #[arg_kind(Event)]
    events: Vec<Event>,
    #[arg_kind(Message)]
    messages: Vec<Message>,
    // --snip--
}
```

## Documentation

[https://docs.rs/ink-analyzer-macro/latest/ink_analyzer_macro/](https://docs.rs/ink-analyzer-macro/latest/ink_analyzer_macro/)

Or you can access documentation locally by running the following command from the project root

```shell
cargo doc -p ink-analyzer-macro --open
```

## Testing

You can run unit tests for all the core functionality by running the following command from the project root

```shell
cargo test -p ink-analyzer-macro
```

## License

Licensed under either [MIT](https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-MIT) or [Apache-2.0](https://github.com/ink-analyzer/ink-analyzer/blob/master/LICENSE-APACHE) license at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
