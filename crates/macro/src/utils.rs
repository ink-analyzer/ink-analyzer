//! ink! analyzer procedural macro utilities.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Data, DeriveInput, Fields, FieldsNamed};

/// Parses a syntax tree for the input token stream, calls derive the derive implementation function
/// and either returns the output token or panics with the supplied error message.
pub fn parse_syntax_tree_and_call_derive_impl(
    input: TokenStream,
    derive_impl: fn(&DeriveInput) -> Option<TokenStream2>,
    error: &str,
) -> TokenStream {
    if let Ok(ast) = syn::parse(input) {
        if let Some(output) = derive_impl(&ast) {
            return output.into();
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

/// Returns true if the list of fields includes a specific field.
pub fn contains_field(fields: &FieldsNamed, name: &str) -> bool {
    fields
        .named
        .iter()
        .filter(|field| {
            if let Some(ident) = &field.ident {
                return ident == name;
            }
            false
        })
        .count()
        == 1
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn parse_struct_fields_works() {
        let ast = syn::parse2(quote! {
            struct Contract {
                ink_attr: InkAttribute,
                ast: Option<Module>,
                syntax: SyntaxNode,
            }
        })
        .unwrap();

        let fields: Vec<String> = parse_struct_fields(&ast)
            .unwrap()
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect();
        assert_eq!(vec!["ink_attr", "ast", "syntax"], fields);
    }

    #[test]
    fn contains_field_works() {
        let ast = syn::parse2(quote! {
            struct Contract {
                ink_attr: InkAttribute,
                ast: Option<Module>,
                syntax: SyntaxNode,
            }
        })
        .unwrap();

        let fields = parse_struct_fields(&ast)
            .expect("`parse_struct_fields` should work. needs to be fixed first.");
        assert!(contains_field(fields, "ink_attr"));
        assert!(!contains_field(fields, "other"));
    }
}
