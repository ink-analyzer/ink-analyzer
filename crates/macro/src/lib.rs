//! Procedural macros for [ink! analyzer](https://docs.rs/ink-analyzer/latest/ink_analyzer/).

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
    utils::parse_syntax_tree_and_call_derive_impl(
        input,
        from_ast::impl_from_ast,
        "#[derive(FromAST)] can only be applied to a `struct` with an `ast` field.",
    )
}

/// Derive macro that implements the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
///
/// # Example
/// ```
/// use ink_analyzer_macro::FromInkAttribute;
/// use ink_analyzer_ir::{FromInkAttribute, InkAttrData, InkAttribute};
/// use ink_analyzer_ir::ast::Module;
///
/// #[derive(FromInkAttribute)]
/// struct Contract {
///     ink_attr: InkAttrData<Module>,
/// }
/// ```
#[proc_macro_derive(FromInkAttribute)]
pub fn from_ink_attribute_derive(input: TokenStream) -> TokenStream {
    utils::parse_syntax_tree_and_call_derive_impl(
        input,
        from_ink_attribute::impl_from_ink_attribute,
        "#[derive(FromInkAttribute)] can only be applied to a `struct` with an `ink_attr` field.",
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
    utils::parse_syntax_tree_and_call_derive_impl(
        input,
        from_syntax::impl_from_syntax,
        "#[derive(FromSyntax)] can only be applied to a `struct` with a `syntax`, `ast` or `ink_attr` field.",
    )
}
