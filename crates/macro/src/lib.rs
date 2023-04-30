//! Procedural macros for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).
//!
//! # Example
//! Using custom derive macros for the `FromInkAttribute` and `FromSyntax` traits to create a `Contract` type.
//!
//! ```
//! use ink_analyzer_macro::{FromInkAttribute, FromSyntax};
//! use ink_analyzer_ir::{Event, FromInkAttribute, FromSyntax, InkAttrData, InkAttribute, Message};
//! use ink_analyzer_ir::ast::Module;
//!
//! #[derive(FromInkAttribute, FromSyntax)]
//! struct Contract {
//!     #[macro_kind(Contract)]
//!     ink_attr: InkAttrData<Module>,
//!     #[arg_kind(Event)]
//!     events: Vec<Event>,
//!     #[arg_kind(Message)]
//!     messages: Vec<Message>,
//!     // --snip--
//! }
//! ```

use proc_macro::TokenStream;

mod from_ast;
mod from_ink_attribute;
mod from_syntax;
mod utils;

/// Derive macro that implements the `FromAST` trait for any `struct` with an `ast` field.
///
/// # Example
/// ```
/// use ink_analyzer_macro::FromAST;
/// use ink_analyzer_ir::FromAST;
/// use ink_analyzer_ir::ast::Attr;
///
/// #[derive(FromAST)]
/// struct InkAttribute {
///     ast: Attr,
/// }
/// ```
#[proc_macro_derive(FromAST)]
pub fn from_ast_derive(input: TokenStream) -> TokenStream {
    utils::parse_syntax_tree_and_call_derive_impl(input, from_ast::impl_from_ast)
}

/// Derive macro that implements the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
///
/// # Attributes
/// All struct fields must be annotated with an attribute that indicates their ink! attribute kind.
///
/// The format of the attribute is `attr_kind(attr_type)` where:
///
/// `attr_kind` is one of:
/// - `macro_kind` - for ink! attribute macros e.g `#[ink::contract]`.
/// - `arg_kind` - for ink! attributes arguments e.g `#[ink(event)]`.
///
/// And `attr_type` is an IR type e.g:
/// - `Contract`, `ChainExtension`e.t.c for `macro_kind` attributes.
/// - `Storage`, `Event`, `Constructor` e.t.c for `arg_kind` attributes.
///
/// Apart from the `ink_attr` which is required,
/// all other fields are optional and can use any valid identifier.
/// However, if present, they must be `Vec`s of an IR type that matches the `attr_type` of their annotated attribute.
///
/// # Example
/// ```
/// use ink_analyzer_macro::FromInkAttribute;
/// use ink_analyzer_ir::{Event, FromInkAttribute, InkAttrData, InkAttribute};
/// use ink_analyzer_ir::ast::Module;
///
/// #[derive(FromInkAttribute)]
/// struct Contract {
///     // Required `ink_attr` field.
///     #[macro_kind(Contract)]
///     ink_attr: InkAttrData<Module>,
///     // Optional ink! events field whose IR type matches it's annotated `attr_type`.
///     #[arg_kind(Event)]
///     events: Vec<Event>,
/// }
/// ```
#[proc_macro_derive(FromInkAttribute, attributes(macro_kind, arg_kind))]
pub fn from_ink_attribute_derive(input: TokenStream) -> TokenStream {
    utils::parse_syntax_tree_and_call_derive_impl(
        input,
        from_ink_attribute::impl_from_ink_attribute,
    )
}

/// Derive macro that implements the `FromSyntax` trait
/// for any `struct` with a `syntax`, `ast` or `ink_attr` field.
///
/// # Example
/// ```
/// use ink_analyzer_macro::FromSyntax;
/// use ink_analyzer_ir::FromSyntax;
/// use ink_analyzer_ir::syntax::SyntaxNode;
///
/// #[derive(FromSyntax)]
/// struct Contract {
///     syntax: SyntaxNode,
/// }
/// ```
#[proc_macro_derive(FromSyntax)]
pub fn from_syntax_derive(input: TokenStream) -> TokenStream {
    utils::parse_syntax_tree_and_call_derive_impl(input, from_syntax::impl_from_syntax)
}
