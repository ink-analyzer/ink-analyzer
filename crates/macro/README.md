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
Using `ink_analyzer_macro::entity` proc-macro to create a `Contract` type.

```rust
use ink_analyzer_ir::{Event, Message, Storage};
use ink_analyzer_ir::ast;

#[ink_analyzer_macro::entity(macro_kind = Contract)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Contract {
    ast: ast::Module,
    storage: Option<Storage>,
    events: Vec<Event>,
    #[initializer(call = ink_analyzer_ir::ink_callable_closest_descendants)]
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
