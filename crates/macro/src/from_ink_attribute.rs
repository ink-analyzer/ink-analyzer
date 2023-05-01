use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Attribute, DeriveInput, Field, Path, Type};

use crate::utils;

/// Returns an implementation of the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
pub fn impl_from_ink_attribute(ast: &DeriveInput) -> syn::Result<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = utils::parse_struct_fields(ast) {
        let ir_crate_path = utils::get_normalized_ir_crate_path();
        let mut ink_attr_field_config: Option<FieldConfig> = None;

        let mut field_values: Vec<TokenStream> = Vec::new();
        let mut field_errors: Option<syn::Error> = None;

        for field in fields.named.iter() {
            if let Some(ident) = &field.ident {
                if let Some(field_config) = FieldConfig::build(field) {
                    if ident == "ink_attr" {
                        // Field value for `ink_attr` field is handled differently and has to be the last.
                        ink_attr_field_config = Some(field_config);
                    } else {
                        let kind_type_variant = field_config.kind_type_variant; // e.g `Contract`, `Storage` e.t.c.
                        let mut field_value = if kind_type_variant == syn::parse_quote! { Impl } {
                            // ink! impl are a special case because the attribute is actually optional
                            // and validity is determined based on context of descendants.
                            quote! {
                                #ir_crate_path::ink_impl_closest_descendants(ink_attr_data.parent_syntax())
                            }
                        } else if kind_type_variant == syn::parse_quote! { Constructor }
                            || kind_type_variant == syn::parse_quote! { Message }
                        {
                            // ink! `constructors` and ink! `messages` are also special because
                            // they're ink! impl children, so we also look inside `impl` blocks to find them.
                            quote! {
                                #ir_crate_path::ink_callable_closest_descendants(ink_attr_data.parent_syntax())
                            }
                        } else {
                            // For everything else, we simply return closest ink! descendants .
                            quote! {
                                #ir_crate_path::ink_closest_descendants(ink_attr_data.parent_syntax())
                            }
                        };

                        if is_option_type(&field.ty) {
                            field_value = quote! {
                                #field_value.into_iter().next()
                            }
                        }

                        field_values.push(quote! {
                            #ident: #field_value
                        });
                    }
                } else {
                    let error = syn::Error::new(
                        ident.span(),
                        format!("`{ident}` field must be annotated with ink! attribute kind info e.g `#[macro_kind(Contract)]`"),
                    );
                    match &mut field_errors {
                        Some(combined_error) => combined_error.combine(error),
                        None => field_errors = Some(error),
                    }
                }
            }
        }

        if let Some(combined_error) = field_errors {
            // Return combined field errors.
            return Err(combined_error);
        }

        if let Some(config) = ink_attr_field_config {
            // Generate implementation code.
            // `ink_attr_field_config` is only set if an `ink_attr` field is present.
            let kind_variant = config.kind_variant;
            let kind_type = config.kind_type;
            let kind_type_variant = config.kind_type_variant;

            return Ok(quote! {
                impl FromInkAttribute for #name {
                    fn ink_attr(&self) -> &InkAttribute {
                        &self.ink_attr.attr()
                    }

                    fn can_cast(attr: &InkAttribute) -> bool {
                        *attr.kind() == #ir_crate_path::InkAttributeKind::#kind_variant(#ir_crate_path::#kind_type::#kind_type_variant)
                    }

                    fn cast(attr: #ir_crate_path::InkAttribute) -> Option<Self> {
                        Self::can_cast(&attr).then(|| {
                            let ink_attr_data = #ir_crate_path::InkAttrData::from(attr);
                            Self {
                                #( #field_values, )*
                                ink_attr: ink_attr_data,
                            }
                        })
                    }
                }
            });
        }
    }

    Err(syn::Error::new(
        ast.span(),
        "#[derive(FromInkAttribute)] can only be applied to a `struct` with an `ink_attr` field.",
    ))
}

fn get_ink_field_kind_attr(field: &Field) -> Option<&Attribute> {
    if let Some(attr) = utils::find_attribute_by_path(&field.attrs, "macro_kind") {
        return Some(attr);
    }
    utils::find_attribute_by_path(&field.attrs, "arg_kind")
}

fn is_option_type(field_type: &Type) -> bool {
    match field_type {
        Type::Path(type_path) => {
            if let Some(first_segment) = &type_path.path.segments.first() {
                first_segment.ident == "Option"
            } else {
                false
            }
        }
        _ => false,
    }
}

struct FieldConfig {
    kind_variant: TokenStream,
    kind_type: TokenStream,
    kind_type_variant: Path,
}

impl FieldConfig {
    pub fn build(ink_attr_field: &Field) -> Option<Self> {
        let field_kind_attr = get_ink_field_kind_attr(ink_attr_field)?;
        let (kind_variant, kind_type) = if field_kind_attr.path().is_ident("macro_kind") {
            (quote! { Macro }, quote! { InkMacroKind })
        } else {
            (quote! { Arg }, quote! { InkArgKind })
        };
        let kind_type_variant = field_kind_attr.clone().parse_args::<Path>().ok()?;

        Some(Self {
            kind_variant,
            kind_type,
            kind_type_variant,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils;
    use quote::format_ident;
    use syn::{Ident, ItemImpl};

    fn expected_impl(name: Ident) -> ItemImpl {
        let ir_crate_path = utils::get_normalized_ir_crate_path();

        syn::parse_quote! {
            impl FromInkAttribute for #name {
                fn ink_attr(&self) -> &InkAttribute {
                    &self.ink_attr.attr()
                }

                fn can_cast(attr: &InkAttribute) -> bool {
                    *attr.kind() == #ir_crate_path::InkAttributeKind::Macro(#ir_crate_path::InkMacroKind::Contract)
                }

                fn cast(attr: #ir_crate_path::InkAttribute) -> Option<Self> {
                    Self::can_cast(&attr).then(|| {
                        let ink_attr_data = #ir_crate_path::InkAttrData::from(attr);
                        Self {
                            ink_attr: ink_attr_data,
                        }
                    })
                }
            }
        }
    }

    fn parse_actual_impl(input: DeriveInput) -> ItemImpl {
        syn::parse2::<ItemImpl>(impl_from_ink_attribute(&input).unwrap()).unwrap()
    }

    #[test]
    fn struct_with_ink_attr_field_works() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                #[macro_kind(Contract)]
                ink_attr: InkAttrData<Module>,
            }
        };

        assert_eq!(parse_actual_impl(input), expected_impl(name));
    }

    #[test]
    fn struct_without_ink_attr_field_fails() {
        let name = format_ident!("Contract");
        let input = syn::parse_quote! {
            struct #name {
                other: String,
            }
        };

        let output = impl_from_ink_attribute(&input);

        assert!(output.is_err());
    }
}
