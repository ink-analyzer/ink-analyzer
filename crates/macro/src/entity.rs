use darling::ast::NestedMeta;
use darling::{FromAttributes, FromMeta};
use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

use crate::error::Error;
use crate::utils;

/// Returns an implementation of the [`InkEntity`] trait for any `struct` with an `ast` field where the type for `ast` is `T: ASTNode`.
pub fn impl_entity(args: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    let struct_item: syn::ItemStruct =
        syn::parse2(item).map_err(|err| syn::Error::new(err.span(), ITEM_KIND_ERROR))?;
    let ir_crate_path = utils::get_normalized_ir_crate_path();

    let attr_span = args.span();
    let attr_args = NestedMeta::parse_meta_list(args)?;
    let config = match attr_args.len() {
        0 => Config::default(),
        1 => Config::from_list(&attr_args).map_err(Error::from)?,
        _ => return Err(syn::Error::new(attr_span, ARGUMENT_ERROR).into()),
    };

    let mut ast_field: Option<&syn::Field> = None;
    let mut descendant_fields: Vec<TokenStream> = Vec::new();
    let mut initializers: Vec<TokenStream> = Vec::new();
    let mut getters: Vec<TokenStream> = Vec::new();
    let mut field_errors: Vec<syn::Error> = Vec::new();

    let mut add_field_error = |field_type: &syn::Type| {
        field_errors.push(syn::Error::new(field_type.span(), FIELD_TYPE_ERROR))
    };

    macro_rules! attr_kind {
        ($value: expr, $macro_variant: path, $kind_type_variant: ident) => {
            {
                let (kind_variant, kind_type) = if let $macro_variant(_) = &$value {
                    (quote! { Macro }, quote! { InkMacroKind })
                } else {
                    (quote! { Arg }, quote! { InkArgKind })
                };
                quote! {
                    #ir_crate_path::InkAttributeKind::#kind_variant(#ir_crate_path::#kind_type::#$kind_type_variant)
                }
            }
        };
    }

    match &struct_item.fields {
        syn::Fields::Named(fields) => {
            for field in &fields.named {
                if let Some(field_name) = &field.ident {
                    if field_name == "ast" {
                        ast_field = Some(field);
                    } else {
                        let field_type = &field.ty;
                        // Creates non-ast fields (along with their non-initializer attributes).
                        let other_field_attrs = field
                            .attrs
                            .iter()
                            .filter(|attr| !attr.path().is_ident("initializer"));
                        descendant_fields.push(quote! {
                            #( #other_field_attrs )*
                            #field_name: #field_type
                        });

                        // Creates initializer, return type and expressions for getters.
                        match &field_type {
                            syn::Type::Path(type_path) => {
                                let field_type_info =
                                    type_path.path.segments.first().and_then(|segment| {
                                        match &segment.arguments {
                                            syn::PathArguments::AngleBracketed(it) => {
                                                let base_type = &it.args;
                                                Some((&segment.ident, quote! { #base_type }))
                                            }
                                            _ => None,
                                        }
                                    });
                                match field_type_info {
                                    Some((base_type, base_field_type)) => {
                                        let field_config = FieldConfig::build(field)?;
                                        let initializer = match &field_config {
                                            FieldConfig::Closest(_) => quote! {
                                                #ir_crate_path::ink_closest_descendants(&root_node)
                                            },
                                            FieldConfig::PeekMacro(kind_type_variant)
                                            | FieldConfig::PeekArg(kind_type_variant) => {
                                                let attr_kind = attr_kind!(
                                                    field_config,
                                                    FieldConfig::PeekMacro,
                                                    kind_type_variant
                                                );
                                                quote! {
                                                    #ir_crate_path::ink_peekable_quasi_closest_descendants(&root_node, |attr| {
                                                        *attr.kind() == #attr_kind
                                                    })
                                                }
                                            }
                                            FieldConfig::Call(fn_call) => quote! {
                                                #fn_call(&root_node)
                                            },
                                        };

                                        let result = if base_type == "Vec" {
                                            Some((
                                                quote! { collect },
                                                quote! { &[#base_field_type] },
                                                quote! { &self.#field_name },
                                            ))
                                        } else if base_type == "Option" {
                                            Some((
                                                quote! { next },
                                                quote! { Option<&#base_field_type> },
                                                quote! { self.#field_name.as_ref() },
                                            ))
                                        } else {
                                            None
                                        };
                                        match result {
                                            Some((consumer, ret_type, ret_expr)) => {
                                                initializers.push(quote! {
                                                    #field_name: #initializer.#consumer()
                                                });

                                                let comment = format!(
                                                    "Returns ink! {}.",
                                                    field_name.to_string().replace('_', " ")
                                                );
                                                getters.push(quote! {
                                                    #[doc = #comment]
                                                    pub fn #field_name(&self) -> #ret_type {
                                                        #ret_expr
                                                    }
                                                });
                                            }
                                            None => add_field_error(field_type),
                                        }
                                    }
                                    None => add_field_error(field_type),
                                }
                            }
                            _ => add_field_error(field_type),
                        }
                    }
                }
            }
        }
        syn::Fields::Unit => (),
        syn::Fields::Unnamed(_) => {
            return Err(syn::Error::new(struct_item.span(), ITEM_KIND_ERROR).into())
        }
    }

    if !field_errors.is_empty() {
        let combined_error =
            field_errors[1..]
                .iter()
                .fold(field_errors[0].clone(), |mut acc, err| {
                    acc.combine(err.clone());
                    acc
                });
        return Err(combined_error.into());
    }

    let ast_type = &ast_field
        .ok_or(syn::Error::new(struct_item.span(), ITEM_KIND_ERROR))?
        .ty;

    let name = &struct_item.ident;
    let vis = &struct_item.vis;
    let generics = &struct_item.generics;
    let attrs = struct_item.attrs;

    let (can_cast_impl, ink_attr_init) = match &config {
        Config::AST => (
            quote! {
                use #ir_crate_path::syntax::AstNode;
                <#ast_type>::can_cast(node.kind())
            },
            quote! { None },
        ),
        Config::MacroKind(kind_type_variant) | Config::ArgKind(kind_type_variant) => {
            let attr_kind = attr_kind!(config, Config::MacroKind, kind_type_variant);
            (
                quote! {
                    use #ir_crate_path::syntax::AstNode;
                    if #ir_crate_path::ast::Attr::can_cast(node.kind()) {
                        #ir_crate_path::ast::Attr::cast(node.clone())
                            .and_then(#ir_crate_path::InkAttribute::cast)
                            .map_or(false, |attr| *attr.kind() == #attr_kind)
                    } else {
                        #ir_crate_path::ink_attrs(node).any(|attr| *attr.kind() == #attr_kind)
                    }
                },
                quote! {
                    if #ir_crate_path::ast::Attr::can_cast(root_node.kind()) {
                        #ir_crate_path::ast::Attr::cast(root_node.clone())
                            .and_then(#ir_crate_path::InkAttribute::cast)
                            .filter(|attr| *attr.kind() == #attr_kind)
                    } else {
                        #ir_crate_path::ink_attrs(&root_node).find(|attr| *attr.kind() == #attr_kind)
                    }
                },
            )
        }
        Config::Call(fn_call) => (quote! { #fn_call(&node) }, quote! { None }),
    };

    let root_node_decl = quote! {
        let root_node = if #ir_crate_path::ast::Attr::can_cast(node.kind()) {
            node.parent().unwrap_or(node)
        } else {
            node
        };
    };

    let self_def = quote! {
        Self {
            #( #initializers, )*
            syntax: root_node.clone(),
            ast: <#ast_type>::cast(root_node.clone()),
            ink_attr: #ink_attr_init,
        }
    };

    let (cast_impl, from_impl) = match &config {
        Config::AST => (
            quote! {
                use #ir_crate_path::syntax::AstNode;
                <#ast_type>::cast(node)
                    .map(|ast_node| <Self as From<#ast_type>>::from(ast_node))
            },
            quote! {
                impl #generics From<#ast_type> for #name #generics {
                    fn from(value: #ast_type) -> Self {
                        use #ir_crate_path::syntax::AstNode;
                        let node = value.syntax().clone();

                        #root_node_decl

                        #self_def
                    }
                }
            },
        ),
        _ => (
            quote! {
                if Self::can_cast(&node) {
                    use #ir_crate_path::syntax::AstNode;
                    #root_node_decl

                    Some(#self_def)
                } else {
                    None
                }
            },
            quote! {},
        ),
    };

    Ok(quote! {
        #( #attrs )*
        #vis struct #name #generics {
            syntax: #ir_crate_path::syntax::SyntaxNode,
            ast: Option<#ast_type>,
            ink_attr: Option<#ir_crate_path::InkAttribute>,
            #( #descendant_fields, )*
        }

        #from_impl

        impl #ir_crate_path::InkEntity for #name #generics {
            type AST = #ast_type;

            fn can_cast(node: &#ir_crate_path::syntax::SyntaxNode) -> bool {
                #can_cast_impl
            }

            fn cast(node: #ir_crate_path::syntax::SyntaxNode) -> Option<Self> {
                #cast_impl
            }

            fn syntax(&self) -> &#ir_crate_path::syntax::SyntaxNode {
                &self.syntax
            }

            fn ast(&self) -> Option<&Self::AST> {
                self.ast.as_ref()
            }

            fn ink_attr(&self) -> Option<&#ir_crate_path::InkAttribute> {
                return self.ink_attr.as_ref();
            }
        }

        impl #name {
            #( #getters )*
        }
    })
}

const ITEM_KIND_ERROR: &str =
    "`#[ink_analyzer_macro::entity]` can only be applied to a `struct` with an `ast` field.";
const ARGUMENT_ERROR: &str =
    "`#[ink_analyzer_macro::entity(..)]` takes only a single argument of either `ast`, `macro_kind`, `arg_kind` or `fn`.";
const FIELD_ARGUMENT_ERROR: &str =
    "`#[initializer(..)]` takes only a single argument of either `closest`, `peek_macro`, `peek_arg` or `fn`.";
const FIELD_TYPE_ERROR: &str =
    "Field types must be of the form `Vec<T>` or `Option<T>` where `T: InkEntity`";

#[derive(Debug, Default, FromMeta)]
#[darling(default)]
#[allow(clippy::upper_case_acronyms)]
enum Config {
    #[default]
    #[darling(rename = "ast")]
    AST,
    MacroKind(syn::Ident),
    ArgKind(syn::Ident),
    Call(syn::Path),
}

#[derive(Debug)]
enum FieldConfig {
    Closest(darling::util::Flag),
    PeekMacro(syn::Ident),
    PeekArg(syn::Ident),
    Call(syn::Path),
}

impl FieldConfig {
    fn build(field: &syn::Field) -> Result<Self, Error> {
        let attrs = &field.attrs;
        let field_config = FieldConfigInternal::from_attributes(attrs)?;

        match (
            field_config.closest.is_present(),
            field_config.peek_macro,
            field_config.peek_arg,
            field_config.call,
        ) {
            // Default.
            (false, None, None, None) => Ok(Self::default()),
            // Exactly one argument set.
            (true, None, None, None) => Ok(Self::Closest(darling::util::Flag::present())),
            (false, Some(name), None, None) => Ok(Self::PeekMacro(name)),
            (false, None, Some(name), None) => Ok(Self::PeekArg(name)),
            (false, None, None, Some(path)) => Ok(Self::Call(path)),
            // More than one argument set.
            _ => Err(Error::Syn(syn::Error::new(
                field.span(),
                FIELD_ARGUMENT_ERROR,
            ))),
        }
    }
}

impl Default for FieldConfig {
    fn default() -> Self {
        Self::Closest(darling::util::Flag::present())
    }
}

#[derive(Debug, FromAttributes)]
#[darling(attributes(initializer))]
struct FieldConfigInternal {
    closest: darling::util::Flag,
    peek_macro: Option<syn::Ident>,
    peek_arg: Option<syn::Ident>,
    call: Option<syn::Path>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ast_only_works() {
        let item = quote! {
            struct Contract {
                ast: ra_ap_syntax::Module,
            }
        };
        let result = impl_entity(quote! {}, item);
        assert!(result.is_ok());
    }

    #[test]
    fn descendant_fields_works() {
        let item = quote! {
            struct Contract {
                ast: ra_ap_syntax::Module,
                storage: Option<ink_analyzer_ir::Storage>,
                constructors: Vec<ink_analyzer_ir::Constructor>,
                messages: Vec<ink_analyzer_ir::Message>,
            }
        };
        let result = impl_entity(quote! {}, item);
        assert!(result.is_ok());
    }

    #[test]
    fn macro_kind_entity_works() {
        let args = quote! {
            macro_kind = Contract
        };
        let item = quote! {
            struct Contract {
                ast: ra_ap_syntax::Module,
                storage: Option<ink_analyzer_ir::Storage>,
                constructors: Vec<ink_analyzer_ir::Constructor>,
                messages: Vec<ink_analyzer_ir::Message>,
            }
        };
        let result = impl_entity(args, item);
        assert!(result.is_ok());
    }

    #[test]
    fn arg_kind_entity_works() {
        let args = quote! {
            arg_kind = Message
        };
        let item = quote! {
            struct Message {
                ast: ra_ap_syntax::Fn,
            }
        };
        let result = impl_entity(args, item);
        assert!(result.is_ok());
    }

    #[test]
    fn call_entity_works() {
        let args = quote! {
            call = self::can_cast
        };
        let item = quote! {
            struct InkImpl {
                ast: ra_ap_syntax::Impl,
                constructors: Vec<ink_analyzer_ir::Constructor>,
                messages: Vec<ink_analyzer_ir::Message>,
            }
        };
        let result = impl_entity(args, item);
        assert!(result.is_ok());
    }

    #[test]
    fn initializer_attributes_works() {
        let item = quote! {
            struct Contract {
                ast: ra_ap_syntax::Module,
                #[initializer(closest)]
                storage: Option<ink_analyzer_ir::Storage>,
                #[initializer(peek_arg = Impl)]
                constructors: Vec<ink_analyzer_ir::Constructor>,
                #[initializer(call = self::init)]
                messages: Vec<ink_analyzer_ir::Message>,
                // For demo purpose.
                #[initializer(peek_macro = TraitDefinition)]
                tests: Vec<ink_analyzer_ir::InkTest>,
            }
        };
        let result = impl_entity(quote! {}, item);
        assert!(result.is_ok());
    }

    #[test]
    fn no_ast_field_fails() {
        for item in [
            quote! {
                struct Contract;
            },
            quote! {
                struct Contract(u8);
            },
            quote! {
                struct Contract {
                    storage: Option<ink_analyzer_ir::Storage>,
                }
            },
        ] {
            let result = impl_entity(quote! {}, item);
            assert!(result.is_err());
        }
    }

    #[test]
    fn invalid_macro_args_fails() {
        for args in [
            // Unknown
            quote! {
                xyz
            },
            // Multiple
            quote! {
                ast, macro_kind = Contract
            },
            quote! {
                ast, arg_kind = Message
            },
            quote! {
                ast, call = self::can_cast
            },
        ] {
            let result = impl_entity(
                args,
                quote! {
                    struct Contract {
                        ast: ra_ap_syntax::Module,
                    }
                },
            );
            assert!(result.is_err());
        }
    }

    #[test]
    fn invalid_initializer_args_fails() {
        for args in [
            // Unknown
            quote! {
                xyz
            },
            // Multiple
            quote! {
                closest, peek_macro = Contract
            },
            quote! {
                closest, peek_arg = Message
            },
            quote! {
                closest, call = self::init
            },
        ] {
            let result = impl_entity(
                quote! {},
                quote! {
                    struct Contract {
                        ast: ra_ap_syntax::Module,
                        #[initializer(#args)]
                        storage: Option<ink_analyzer_ir::Storage>,
                    }
                },
            );
            assert!(result.is_err());
        }
    }
}
