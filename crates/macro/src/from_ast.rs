use crate::utils::parse_struct_fields;
use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

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
            return Some(gen.into());
        }
    }

    None
}

// TODO: Add unit tests
