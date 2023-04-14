use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::utils::parse_struct_fields;

/// Returns an implementation of the `FromSyntax` trait for any `struct` with a `syntax` field.
pub fn impl_from_syntax_attribute(ast: &DeriveInput) -> Option<TokenStream> {
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
            return Some(gen.into());
        }
    }

    None
}

// TODO: Add unit tests
