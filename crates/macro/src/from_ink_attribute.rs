use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, DeriveInput, Field, Path};

use crate::utils;

/// Returns an implementation of the `FromInkAttribute` trait for any `struct` with an `ink_attr` field.
pub fn impl_from_ink_attribute(ast: &DeriveInput) -> Option<TokenStream> {
    let name = &ast.ident;

    if let Some(fields) = utils::parse_struct_fields(ast) {
        if let Some(ink_attr_field) = utils::find_field(fields, "ink_attr") {
            let ir_crate_path = get_ir_crate_path();

            let field_config = FieldConfig::build(ink_attr_field)
                .expect("`ink_attr` must be annotated with kind info e.g [path_kind(Contract)]");

            let kind_variant = field_config.kind_variant;
            let kind_type = field_config.kind_type;
            let kind_type_variant = field_config.kind_type_variant;

            let field_values: Vec<TokenStream> = fields.named.iter().filter_map(|field| {
                if let Some(ident) = &field.ident {
                    if ident != "ink_attr" {
                        let field_config = FieldConfig::build(field)
                            .expect("`{ident}` must be annotated with kind info e.g [path_kind(Contract)]");
                        let kind_variant = field_config.kind_variant;
                        let kind_type = field_config.kind_type;
                        let kind_type_variant = field_config.kind_type_variant;

                        return Some(quote! {
                            #ident: #ir_crate_path::ink_attrs_closest_descendants(ink_attr_data.parent_syntax())
                                .into_iter()
                                .filter_map(|item| {
                                    if *item.kind() == #ir_crate_path::InkAttributeKind::#kind_variant(#ir_crate_path::#kind_type::#kind_type_variant) {
                                        return Some(#ir_crate_path::#kind_type_variant::cast(item).expect("Should be able to cast"));
                                    }
                                    None
                                })
                                .collect(),
                        });
                    }
                }
                None
            }).collect();

            let gen = quote! {
                impl FromInkAttribute for #name {
                    fn ink_attr(&self) -> &InkAttribute {
                        &self.ink_attr.attr()
                    }

                    fn can_cast(attr: &InkAttribute) -> bool {
                        *attr.kind() == #ir_crate_path::InkAttributeKind::#kind_variant(#ir_crate_path::#kind_type::#kind_type_variant)
                    }

                    fn cast(attr: #ir_crate_path::InkAttribute) -> Option<Self> {
                        if Self::can_cast(&attr) {
                            let ink_attr_data = #ir_crate_path::InkAttrData::from(attr);

                            return Some(Self {
                                #( #field_values )*
                                ink_attr: ink_attr_data,
                            });
                        }
                        None
                    }
                }
            };
            return Some(gen);
        }
    }

    None
}

fn get_ir_crate_path() -> TokenStream {
    if let Ok(pkg_name) = std::env::var("CARGO_PKG_NAME") {
        if pkg_name == "ink-analyzer-ir" {
            return quote! { crate };
        }
    }
    quote! { ink_analyzer_ir }
}

fn get_ink_field_kind_attr(field: &Field) -> Option<&Attribute> {
    if let Some(attr) = utils::find_attribute_by_path(&field.attrs, "path_kind") {
        return Some(attr);
    }
    utils::find_attribute_by_path(&field.attrs, "arg_kind")
}

struct FieldConfig {
    kind_variant: TokenStream,
    kind_type: TokenStream,
    kind_type_variant: Path,
}

impl FieldConfig {
    pub fn build(ink_attr_field: &Field) -> Option<Self> {
        let field_kind_attr = get_ink_field_kind_attr(ink_attr_field)?;
        let (kind_variant, kind_type) = if field_kind_attr.path().is_ident("path_kind") {
            (quote! { Path }, quote! { InkPathKind })
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
    use quote::format_ident;
    use syn::{Ident, ItemImpl};

    fn expected_impl(name: Ident) -> ItemImpl {
        let ir_crate_path = get_ir_crate_path();

        syn::parse_quote! {
            impl FromInkAttribute for #name {
                fn ink_attr(&self) -> &InkAttribute {
                    &self.ink_attr.attr()
                }

                fn can_cast(attr: &InkAttribute) -> bool {
                    *attr.kind() == #ir_crate_path::InkAttributeKind::Path(#ir_crate_path::InkPathKind::Contract)
                }

                fn cast(attr: #ir_crate_path::InkAttribute) -> Option<Self> {
                    if Self::can_cast(&attr) {
                        let ink_attr_data = #ir_crate_path::InkAttrData::from(attr);
                        return Some(Self {
                            ink_attr: ink_attr_data,
                        });
                    }
                    None
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
                #[path_kind(Contract)]
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

        assert!(output.is_none());
    }
}
