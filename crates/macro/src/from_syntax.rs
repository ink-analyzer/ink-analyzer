use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

use crate::utils;

/// Returns an implementation of the `FromSyntax` trait
/// for any `struct` with a `syntax`, `ast` or `ink_attr` field.
pub fn impl_from_syntax(ast: &syn::DeriveInput) -> syn::Result<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = utils::parse_struct_fields(ast) {
        let expr: Option<TokenStream> = if utils::contains_field(fields, "syntax") {
            Some(syntax_field_return_expr())
        } else if utils::contains_field(fields, "ast") {
            Some(ast_field_return_expr())
        } else if utils::contains_field(fields, "ink_attr") {
            Some(ink_attr_field_return_expr())
        } else {
            None
        };

        let ir_crate_path = utils::get_normalized_ir_crate_path();
        if let Some(return_expr) = expr {
            return Ok(quote! {
                impl FromSyntax for #name {
                    fn syntax(&self) -> &#ir_crate_path::syntax::SyntaxNode {
                        #return_expr
                    }
                }
            });
        }
    }

    Err(syn::Error::new(ast.span(), "#[derive(FromSyntax)] can only be applied to a `struct` with a `syntax`, `ast` or `ink_attr` field."))
}

fn syntax_field_return_expr() -> TokenStream {
    quote! { &self.syntax }
}

fn ast_field_return_expr() -> TokenStream {
    quote! { &self.ast.syntax() }
}

fn ink_attr_field_return_expr() -> TokenStream {
    quote! { &self.ink_attr.parent_syntax() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::format_ident;
    use syn::{Ident, ItemImpl};

    fn expected_impl(name: Ident, return_expr: TokenStream) -> ItemImpl {
        let ir_crate_path = utils::get_normalized_ir_crate_path();
        syn::parse_quote! {
            impl FromSyntax for #name {
                fn syntax(&self) -> &#ir_crate_path::syntax::SyntaxNode {
                    #return_expr
                }
            }
        }
    }

    fn parse_actual_impl(input: syn::DeriveInput) -> ItemImpl {
        syn::parse2::<ItemImpl>(impl_from_syntax(&input).unwrap()).unwrap()
    }

    #[test]
    fn struct_with_syntax_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                syntax: SyntaxNode,
            }
        };

        assert_eq!(
            parse_actual_impl(input),
            expected_impl(name, syntax_field_return_expr())
        );
    }

    #[test]
    fn struct_with_ast_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                ast: Module,
            }
        };

        assert_eq!(
            parse_actual_impl(input),
            expected_impl(name, ast_field_return_expr())
        );
    }

    #[test]
    fn struct_with_ink_attr_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                ink_attr: InkAttrData<Module>,
            }
        };

        assert_eq!(
            parse_actual_impl(input),
            expected_impl(name, ink_attr_field_return_expr())
        );
    }

    #[test]
    fn struct_with_none_of_expected_fields_fails() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                other: String,
            }
        };

        let output = impl_from_syntax(&input);

        assert!(output.is_err());
    }
}
