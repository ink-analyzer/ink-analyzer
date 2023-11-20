//! Procedural macros for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).
//!
//! # Example
//! Using `ink_analyzer_macro::entity` proc-macro to create a `Contract` type.
//!
//! ```
//! use ink_analyzer_ir::{Event, Message, Storage};
//! use ink_analyzer_ir::ast;
//!
//! #[ink_analyzer_macro::entity(macro_kind = Contract)]
//! #[derive(Debug, Clone, PartialEq, Eq)]
//! struct Contract {
//!     ast: ast::Module,
//!     storage: Option<Storage>,
//!     events: Vec<Event>,
//!     #[initializer(call = ink_analyzer_ir::ink_callable_closest_descendants)]
//!     messages: Vec<Message>,
//!     // --snip--
//! }
//! ```

use proc_macro::TokenStream;

mod entity;
mod error;
mod utils;

/// proc-macro that implements the `InkEntity` trait for any `struct` with an `ast` field,
/// where the type for `ast` is `T: ASTNode`.
///
/// **NOTE:** Any additional macros should be applied after this macro
/// because it modifies the fields of the struct to which it is applied.
///
/// # Arguments
/// The `entity` macro takes one argument that represents the casting precondition as follows:
///
/// - `ast` - accepts on syntax nodes whose kind matches the type of the `ast` field.
///           This is the default and be skipped.
/// - `macro_kind = T` - accepts only items annotated with an ink! attribute macro of the kind `T`
///                      (e.g. `Contract` for `#[ink::contract]`).
/// - `arg_kind = T` - accepts only items annotated with an ink! attribute argument of the kind `T`
///                    (e.g. `Storage` for `#[ink(storage)]`).
/// - `call = F` - accepts only syntax nodes for which the function `F` returns true
///                where `F: Fn(&SyntaxNode) -> bool`.
///
/// # Attributes
/// Apart from the `ast` field which is required and must be `T: ASTNode`,
/// all other fields are optional and can use any valid identifier.
/// However, if present, they must be either `Vec`s or `Option`s of an ink! entity type
/// (e.g. `Contract`, `ChainExtension` for ink! attribute macro types e.t.c
/// or `Storage`, `Event`, `Constructor` for ink! attribute argument types)
/// because they represent ink! entity descendants.
///
/// These ink! entity descendant fields (i.e. all fields apart from the `ast` field)
/// can (optionally) be annotated with a single `#[initializer(...)]` attribute
/// that defines their initialization strategy.
///
/// The `initializer` attribute takes one argument that represents the initialization strategy
/// as follows:
///
/// - `closest` - collects all descendant ink! entities of the field's type that
///               don't have any other ink! entity ancestors between them and the this entity.
///               This is the default and be skipped.
/// - `peek_macro = T` - collects all descendant ink! entities of the field's type that,
///                  either don't have any other ink! entity ancestors
///                  or only have one ink! attribute macro entity of type T
///                  (e.g. `Contract` for `#[ink::contract]`), between them and the this entity.
/// - `peek_arg = T` - collects all descendant ink! entities of the field's type that,
///                  either don't have any other ink! entity ancestors
///                  or only have one ink! attribute argument entity of type T
///                  (e.g. `Storage` for `#[ink(storage)]`), between them and the this entity.
/// - `call = F` - collects all ink! entities returned by the function `F`
///                where `F: Fn(&SyntaxNode) -> impl Iterator<Item = T>` and `T: InkEntity`
///                and `T` also matches the ink! entity type for the field.
///
/// # Example
/// ```
/// use ink_analyzer_ir::{Event, Message, Storage};
/// use ink_analyzer_ir::ast;
///
/// #[ink_analyzer_macro::entity(macro_kind = Contract)]
/// #[derive(Debug, Clone, PartialEq, Eq)]
/// struct Contract {
///     ast: ast::Module,
///     storage: Option<Storage>,
///     events: Vec<Event>,
///     #[initializer(call = ink_analyzer_ir::ink_callable_closest_descendants)]
///     messages: Vec<Message>,
///     // --snip--
/// }
/// ```
#[proc_macro_attribute]
pub fn entity(args: TokenStream, item: TokenStream) -> TokenStream {
    entity::impl_entity(args.into(), item.into())
        .unwrap_or_else(error::Error::into_compile_error)
        .into()
}
