use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::utils::parse_struct_fields;

/// Returns an implementation of the `FromSyntax` trait for any `struct` with a `syntax` field.
pub fn impl_from_syntax(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = parse_struct_fields(ast) {
        let has_syntax = fields
            .named
            .iter()
            .filter(|field| {
                if let Some(ident) = &field.ident {
                    return ident == "syntax";
                }
                false
            })
            .count()
            == 1;

        if has_syntax {
            let gen = quote! {
                impl FromSyntax for #name {
                    fn syntax(&self) -> &SyntaxNode {
                        &self.syntax
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
    use syn::{Ident, ItemImpl};

    fn expected_impl(name: Ident) -> ItemImpl {
        syn::parse2::<ItemImpl>(quote! {
            impl FromSyntax for #name {
                fn syntax(&self) -> &SyntaxNode {
                    &self.syntax
                }
            }
        })
        .unwrap()
    }

    fn parse_actual_impl(input: DeriveInput) -> ItemImpl {
        syn::parse2::<ItemImpl>(impl_from_syntax(&input).unwrap()).unwrap()
    }

    #[test]
    fn struct_with_syntax_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                syntax: SyntaxNode,
            }
        })
        .unwrap();

        assert_eq!(parse_actual_impl(input), expected_impl(name));
    }

    #[test]
    fn struct_without_syntax_field_fails() {
        let name = format_ident!("Contract");
        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                other: String,
            }
        })
        .unwrap();

        let output = impl_from_syntax(&input);

        assert!(output.is_none());
    }
}
