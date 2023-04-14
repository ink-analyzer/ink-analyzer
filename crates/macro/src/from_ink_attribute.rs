use crate::utils;
use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::utils::parse_struct_fields;

/// Returns an implementation of the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
pub fn impl_from_ink_attribute(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = parse_struct_fields(ast) {
        if utils::contains_field(fields, "ink_attr") {
            let gen = quote! {
                impl FromInkAttribute for #name {
                    fn ink_attr(&self) -> &InkAttribute {
                        &self.ink_attr.attr()
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
            impl FromInkAttribute for #name {
                fn ink_attr(&self) -> &InkAttribute {
                    &self.ink_attr.attr()
                }
            }
        })
        .unwrap()
    }

    fn parse_actual_impl(input: DeriveInput) -> ItemImpl {
        syn::parse2::<ItemImpl>(impl_from_ink_attribute(&input).unwrap()).unwrap()
    }

    #[test]
    fn struct_with_ink_attr_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                ink_attr: InkAttrData<Module>,
            }
        })
        .unwrap();

        assert_eq!(parse_actual_impl(input), expected_impl(name));
    }

    #[test]
    fn struct_without_ink_attr_field_fails() {
        let name = format_ident!("Contract");
        let input = syn::parse2::<DeriveInput>(quote! {
            struct #name {
                other: String,
            }
        })
        .unwrap();

        let output = impl_from_ink_attribute(&input);

        assert!(output.is_none());
    }
}
