use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::utils;

/// Returns an implementation of the `FromSyntax` trait
/// for any `struct` with a `syntax`, `ast` or `ink_attr` field.
pub fn impl_from_syntax(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = utils::parse_struct_fields(ast) {
        let mut expr: Option<TokenStream> = None;
        if utils::contains_field(fields, "syntax") {
            expr = Some(syntax_field_return_expr());
        } else if utils::contains_field(fields, "ast") {
            expr = Some(ast_field_return_expr());
        } else if utils::contains_field(fields, "ink_attr") {
            expr = Some(ink_attr_field_return_expr());
        }

        if let Some(return_expr) = expr {
            let gen = quote! {
                impl FromSyntax for #name {
                    fn syntax(&self) -> &SyntaxNode {
                        #return_expr
                    }
                }
            };
            return Some(gen);
        }
    }

    None
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
        syn::parse_quote! {
            impl FromSyntax for #name {
                fn syntax(&self) -> &SyntaxNode {
                    #return_expr
                }
            }
        }
    }

    fn parse_actual_impl(input: DeriveInput) -> ItemImpl {
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

        assert!(output.is_none());
    }
}
