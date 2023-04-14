use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::utils::parse_struct_fields;

/// Returns an implementation of the `FromAST` trait for any `struct` with an `ast` field.
pub fn impl_from_ast(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = parse_struct_fields(ast) {
        let mut field_filter = fields.named.iter().filter_map(|field| {
            if let Some(ident) = &field.ident {
                return (ident == "ast").then_some(&field.ty);
            }
            None
        });

        if let Some(ast_type) = field_filter.next() {
            let gen = quote! {
                impl FromAST for #name {
                    type AST = #ast_type;

                    fn ast(&self) -> &#ast_type {
                        &self.ast
                    }
                }
            };
            return Some(gen);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::format_ident;
    use syn::{Ident, ItemImpl, Type};

    fn expected_impl(name: Ident, ast_type: Type) -> ItemImpl {
        syn::parse2::<ItemImpl>(quote! {
            impl FromAST for #name {
                type AST = #ast_type;

                fn ast(&self) -> &#ast_type {
                    &self.ast
                }
            }
        })
        .unwrap()
    }

    fn parse_actual_impl(input: DeriveInput) -> ItemImpl {
        syn::parse2::<ItemImpl>(impl_from_ast(&input).unwrap()).unwrap()
    }

    #[test]
    fn struct_with_ast_field_works() {
        let name = format_ident!("InkAttribute");
        let ast_type = syn::parse2::<Type>(quote! { Attr }).unwrap();

        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                ast: #ast_type,
            }
        })
        .unwrap();

        assert_eq!(parse_actual_impl(input), expected_impl(name, ast_type));
    }

    #[test]
    fn struct_without_ast_field_fails() {
        let name = format_ident!("InkAttribute");
        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                other: String,
            }
        })
        .unwrap();

        let output = impl_from_ast(&input);

        assert!(output.is_none());
    }
}
