# ink! Analyzer Proc-macros

Procedural macros for [ink-analyzer](/crates/analyzer) and [ink-analyzer-ir](/crates/ir).

**NOTE:** This project is still work in progress, check back over the next few weeks for regular updates.

## Installation

Run the following Cargo command in your project directory

```shell
cargo add ink-analyzer-macro
```

## Usage

### Example: Use the custom derive macro for the `FromInkAttribute` trait.


```rust
use ink_analyzer_macro::FromInkAttribute;
// -- snip --

#[derive(FromInkAttribute)]
struct Contract {
    ink_attr: InkAttribute,
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

This code is released under both MIT and Apache-2.0 licenses.
