//! ink! analyzer procedural macro utilities.

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Fields, FieldsNamed};

/// Parses a syntax tree for the input token stream, calls derive the derive implementation function
/// and either returns the output token or panics with the supplied error message.
pub fn parse_syntax_tree_and_call_derive_impl(
    input: TokenStream,
    derive_impl: fn(&DeriveInput) -> Option<TokenStream>,
    error: &str,
) -> TokenStream {
    if let Ok(ast) = syn::parse(input) {
        if let Some(output) = derive_impl(&ast) {
            return output;
        }
    }
    panic!("{}", error);
}

/// Returns struct fields if any from a syntax tree.
pub fn parse_struct_fields(ast: &DeriveInput) -> Option<&FieldsNamed> {
    if let Data::Struct(data_struct) = &ast.data {
        if let Fields::Named(fields) = &data_struct.fields {
            return Some(fields);
        }
    }
    None
}

// TODO: Add unit tests
