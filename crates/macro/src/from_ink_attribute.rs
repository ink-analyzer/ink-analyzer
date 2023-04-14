use crate::utils::parse_struct_fields;
use proc_macro::TokenStream;
use quote::quote;
use syn::DeriveInput;

/// Returns an implementation of the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
pub fn impl_from_ink_attribute(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = parse_struct_fields(ast) {
        let has_ink_attr = fields
            .named
            .iter()
            .filter(|field| {
                if let Some(ident) = &field.ident {
                    return ident == "ink_attr";
                }
                false
            })
            .count()
            == 1;

        if has_ink_attr {
            let gen = quote! {
                impl FromInkAttribute for #name {
                    fn ink_attr(&self) -> &InkAttribute {
                        &self.ink_attr
                    }
                }
            };
            return Some(gen.into());
        }
    }

    None
}

// TODO: Add unit tests
