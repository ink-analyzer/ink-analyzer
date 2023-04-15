//! ink! analyzer procedural macro utilities.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Attribute, Data, DeriveInput, Field, Fields, FieldsNamed};

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

/// Returns field if the list of fields includes a field with the name.
pub fn find_field<'a>(fields: &'a FieldsNamed, name: &str) -> Option<&'a Field> {
    fields.named.iter().find(|field| {
        if let Some(ident) = &field.ident {
            return ident == name;
        }
        false
    })
}

/// Returns true if the list of fields contains a field with the name.
pub fn contains_field(fields: &FieldsNamed, name: &str) -> bool {
    find_field(fields, name).is_some()
}

/// Returns attribute if the list of attributes includes an attribute with the name.
pub fn find_attribute_by_path<'a>(attrs: &'a [Attribute], name: &str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| attr.path().is_ident(name))
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{ItemStruct, Path};

    #[test]
    fn parse_struct_fields_works() {
        let ast = syn::parse_quote! {
            struct Contract {
                ink_attr: InkAttribute,
                ast: Option<Module>,
                syntax: SyntaxNode,
            }
        };

        let fields: Vec<String> = parse_struct_fields(&ast)
            .unwrap()
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap().to_string())
            .collect();
        assert_eq!(vec!["ink_attr", "ast", "syntax"], fields);
    }

    #[test]
    fn find_field_works() {
        let ast = syn::parse_quote! {
            struct Contract {
                ink_attr: InkAttribute,
                ast: Option<Module>,
                syntax: SyntaxNode,
            }
        };

        let fields = parse_struct_fields(&ast)
            .expect("`parse_struct_fields` should work. needs to be fixed first.");

        let ink_attr_field = find_field(fields, "ink_attr");
        assert!(ink_attr_field.is_some());
        assert_eq!(ink_attr_field.unwrap().ident.as_ref().unwrap(), "ink_attr");

        assert!(find_field(fields, "other").is_none());
    }

    #[test]
    fn contains_field_works() {
        let ast = syn::parse_quote! {
            struct Contract {
                ink_attr: InkAttribute,
                ast: Option<Module>,
                syntax: SyntaxNode,
            }
        };

        let fields = parse_struct_fields(&ast)
            .expect("`parse_struct_fields` should work. needs to be fixed first.");
        assert!(contains_field(fields, "ink_attr"));
        assert!(!contains_field(fields, "other"));
    }

    #[test]
    fn find_attribute_by_path_works() {
        let ast: ItemStruct = syn::parse_quote! {
            #[path_kind(Contract)]
            #[arg_kind(Storage)]
            struct Dummy;
        };

        let path_kind_attr = find_attribute_by_path(&ast.attrs, "path_kind");
        assert!(path_kind_attr.is_some());
        assert!(&path_kind_attr.unwrap().path().is_ident("path_kind"));
        assert!(&path_kind_attr
            .unwrap()
            .parse_args::<Path>()
            .unwrap()
            .is_ident("Contract"));

        assert!(find_attribute_by_path(&ast.attrs, "other").is_none());
    }
}
