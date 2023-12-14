//! ink! chain extension `ErrorCode` diagnostics.

use ink_analyzer_ir::ast::{AstNode, AstToken, HasAttrs, HasDocComments, HasName};
use ink_analyzer_ir::syntax::{SyntaxToken, TextRange};
use ink_analyzer_ir::{ast, ChainExtension, InkEntity};
use itertools::{Either, Itertools};

use super::utils;
use crate::analysis::utils as analysis_utils;
use crate::codegen::snippets::{FROM_STATUS_CODE_IMPL_PLAIN, FROM_STATUS_CODE_IMPL_SNIPPET};
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, TextEdit};

const INK_ENV_CHAIN_EXTENSION_QUALIFIERS: [&str; 2] =
    ["ink::env::chain_extension", "ink_env::chain_extension"];

const SCALE_QUALIFIERS: [&str; 3] = ["scale", "ink::scale", "parity_scale_codec"];

const SCALE_INFO_QUALIFIERS: [&str; 2] = ["scale_info", "ink::scale_info"];

/// Runs all ink! chain extension `ErrorCode` diagnostics.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L957-L976>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L1269-L1274>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L1092-L1094>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    // Ensures that ink! chain extension `ErrorCode` type can be resolved, see `ensure_resolvable` doc.
    if let Some(diagnostic) = ensure_resolvable(chain_extension) {
        results.push(diagnostic);
    }

    // Ensures that ink! chain extension `ErrorCode` type implements the `FromStatusCode` trait,
    // see `ensure_impl_from_status_code` doc.
    if let Some(diagnostic) = ensure_impl_from_status_code(chain_extension) {
        results.push(diagnostic);
    }

    // Ensures that ink! chain extension `ErrorCode` type implements the SCALE code traits,
    // see `ensure_impl_scale_codec_traits` doc.
    if let Some(diagnostic) = ensure_impl_scale_codec_traits(chain_extension) {
        results.push(diagnostic);
    }

    // Ensures no usages of `Self::ErrorCode`, see `ensure_no_self_error_code_usage` doc.
    ensure_no_self_error_code_usage(results, chain_extension);
}

// Ensures that the ink! chain extension `ErrorCode` type can be resolved to an ADT item (i.e. struct, enum or union).
fn ensure_resolvable(chain_extension: &ChainExtension) -> Option<Diagnostic> {
    // Only continue if there's an `ErrorCode` type.
    let error_code_type = chain_extension.error_code()?.ty()?;
    match error_code_adt(chain_extension) {
        // Handles no resolved `ErrorCode` type.
        None => {
            // Determines text range for the `ErrorCode` type value.
            let range = error_code_type.syntax().text_range();

            Some(Diagnostic {
                message: "`ErrorCode` associated type should be a custom type \
                that implements the `ink::env::chain_extension::FromStatusCode` trait."
                    .to_string(),
                range,
                severity: Severity::Error,
                quickfixes: resolution::candidate_adt_by_name_or_external_trait_impl(
                    resolution::path_from_type(&error_code_type).as_ref(),
                    "FromStatusCode",
                    &INK_ENV_CHAIN_EXTENSION_QUALIFIERS,
                    chain_extension.syntax(),
                )
                .as_ref()
                .and_then(resolution::item_path)
                .map(|candidate_path| {
                    // Suggests a resolved path.
                    vec![Action {
                        label: format!(
                            "Replace `{error_code_type}` associated type with `{candidate_path}`."
                        ),
                        kind: ActionKind::QuickFix,
                        range,
                        edits: vec![TextEdit::replace_with_snippet(
                            candidate_path.clone(),
                            range,
                            Some(format!("${{1:{candidate_path}}}")),
                        )],
                    }]
                }),
            })
        }
        // Ignores resolved environment config.
        Some(_) => None,
    }
}

// Ensures that the ink! chain extension `ErrorCode` type implements the `ink::env::chain_extension::FromStatusCode` trait.
fn ensure_impl_from_status_code(chain_extension: &ChainExtension) -> Option<Diagnostic> {
    // Only continue if there's a named `ErrorCode` type.
    let adt = error_code_adt(chain_extension)?;
    let name = adt.name()?.to_string();

    utils::ensure_external_trait_impl(
        &adt,
        (
            "FromStatusCode",
            &INK_ENV_CHAIN_EXTENSION_QUALIFIERS,
            &chain_extension.syntax().ancestors().last()?,
        ),
        "`ErrorCode` associated type must implement the `ink::env::chain_extension::FromStatusCode` trait.".to_string(),
        format!("Add `ink::env::chain_extension::FromStatusCode` implementation for `{name}`."),
        FROM_STATUS_CODE_IMPL_PLAIN.replace("MyErrorCode", &name),
        Some(FROM_STATUS_CODE_IMPL_SNIPPET.replace("MyErrorCode", &name)),
    )
}

// Ensures that the ink! chain extension `ErrorCode` type implements all SCALE code traits.
//
// Ref: <https://docs.substrate.io/reference/scale-codec/>.
//
// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L1092-L1094>.
fn ensure_impl_scale_codec_traits(chain_extension: &ChainExtension) -> Option<Diagnostic> {
    // Only continue if there's an `ErrorCode` type.
    error_code_adt(chain_extension).and_then(|adt| {
        // Standalone derive attribute (if any).
        let standalone_derive_attr = adt.attrs().find(|attr| {
            attr.path()
                .map_or(false, |path| path.to_string().trim() == "derive")
        });

        // Utilities for extracting derive attribute meta items
        let token_tree_to_non_delimited_string = |token_tree: &ast::TokenTree| {
            let r_paren_option = token_tree.r_paren_token();
            token_tree
                .syntax()
                .children_with_tokens()
                .skip(usize::from(token_tree.l_paren_token().is_some()))
                .take_while(|it| {
                    r_paren_option.is_none() || it.as_token() != r_paren_option.as_ref()
                })
                .join("")
        };
        let meta_to_path_list = |meta: &str| {
            meta.replace(' ', "")
                .split(',')
                .filter_map(ink_analyzer_ir::path_from_str)
                .collect::<Vec<_>>()
        };

        // Standalone derive attribute meta item string (if any).
        let standalone_derive_meta = standalone_derive_attr
            .as_ref()
            .and_then(ast::Attr::token_tree)
            .as_ref()
            .map(token_tree_to_non_delimited_string);

        // Extracts derive item paths from both the standalone and conditional derive attributes.
        let standalone_derived_items = standalone_derive_meta.as_deref().map(meta_to_path_list);
        let conditional_derived_items = adt.attrs().find_map(|attr| {
            if attr
                .path()
                .map_or(false, |path| path.to_string().trim() == "cfg_attr")
            {
                attr.token_tree().map(|token_tree| {
                    token_tree
                        .syntax()
                        .children()
                        .filter(|node| {
                            let is_after_derive = || {
                                node.first_token()
                                    .and_then(|token| {
                                        ink_analyzer_ir::closest_non_trivia_token(
                                            &token,
                                            SyntaxToken::prev_token,
                                        )
                                    })
                                    .map_or(false, |token| token.text() == "derive")
                            };
                            ast::TokenTree::can_cast(node.kind()) && is_after_derive()
                        })
                        .filter_map(|node| {
                            ast::TokenTree::cast(node)
                                .as_ref()
                                .map(token_tree_to_non_delimited_string)
                        })
                        .flat_map(|meta| meta_to_path_list(&meta))
                        .collect::<Vec<_>>()
                })
            } else {
                None
            }
        });
        let derived_items = match (standalone_derived_items, conditional_derived_items) {
            (Some(standalone), Some(conditional)) => {
                let mut items = Vec::with_capacity(standalone.len() + conditional.len());
                items.extend(standalone);
                items.extend(conditional);
                Some(items)
            }
            (standalone_option, conditional_option) => standalone_option.or(conditional_option),
        };

        // Finds unimplemented SCALE codec traits.
        let unimplemented_traits: Vec<_> = ([
            ("Encode", &SCALE_QUALIFIERS, "scale::Encode"),
            ("Decode", &SCALE_QUALIFIERS, "scale::Decode"),
            ("TypeInfo", &SCALE_INFO_QUALIFIERS, "scale_info::TypeInfo"),
        ] as [(&str, &[&str], &str); 3])
            .into_iter()
            .filter_map(|(trait_name, qualifiers, trait_path)| {
                // Finds #[derive(...)] trait implementation for the error code type (if any).
                let is_derived = derived_items.as_ref().map_or(false, |item_paths| {
                    item_paths.iter().any(|path| {
                        resolution::is_external_crate_item(
                            trait_name,
                            path,
                            qualifiers,
                            adt.syntax(),
                        )
                    })
                });
                // Finds trait implementation for the error code type (if any).
                let is_implemented = || {
                    adt.name()
                        .as_ref()
                        .map(ToString::to_string)
                        .zip(chain_extension.syntax().ancestors().last())
                        .and_then(|(error_code_name, ref_node)| {
                            resolution::external_trait_impl(
                                trait_name,
                                qualifiers,
                                &ref_node,
                                Some(&error_code_name),
                            )
                        })
                        .is_some()
                };

                (!is_derived && !is_implemented()).then_some(trait_path)
            })
            .collect();

        // Returns diagnostic for unimplemented SCALE code traits (if any).
        (!unimplemented_traits.is_empty()).then(|| {
            // Determines the insert text, range and snippet for adding a derive implementation.
            let trait_paths_plain = unimplemented_traits.join(", ");
            let trait_paths_snippet = unimplemented_traits
                .iter()
                .enumerate()
                .map(|(idx, path)| format!("${{{}:{path}}}", idx + 1))
                .join(", ");
            let trait_paths_display = unimplemented_traits
                .iter()
                .map(|path| format!("`{path}`"))
                .join(", ");
            // Either updates an existing standalone derive attribute or creates a new one.
            let (insert_text, insert_range, insert_snippet) =
                standalone_derive_attr.as_ref().map_or(
                    (
                        format!("#[derive({trait_paths_plain})]"),
                        TextRange::empty(
                            adt.doc_comments_and_attrs()
                                .last()
                                .and_then(|elem| match elem {
                                    Either::Left(attr) => attr.syntax().last_token(),
                                    Either::Right(doc) => Some(doc.syntax().clone()),
                                })
                                // Finds the first non-(attribute/rustdoc/trivia) token for the item.
                                .and_then(|it| {
                                    ink_analyzer_ir::closest_non_trivia_token(
                                        &it,
                                        SyntaxToken::next_token,
                                    )
                                })
                                .as_ref()
                                // Defaults to the start of the custom type.
                                .map_or(adt.syntax().text_range(), SyntaxToken::text_range)
                                .start(),
                        ),
                        format!("#[derive({trait_paths_snippet})]"),
                    ),
                    |attr| {
                        let meta_prefix = standalone_derive_meta.as_ref().map(|meta| {
                            format!(
                                "{meta}{}",
                                if meta.trim_end().ends_with(',') {
                                    ""
                                } else {
                                    ", "
                                }
                            )
                        });
                        (
                            format!(
                                "#[derive({}{trait_paths_plain})]",
                                meta_prefix.as_deref().unwrap_or_default()
                            ),
                            attr.syntax().text_range(),
                            format!(
                                "#[derive({}{trait_paths_snippet})]",
                                meta_prefix.as_deref().unwrap_or_default()
                            ),
                        )
                    },
                );
            // Determines text range for item "declaration" (fallbacks to range of the entire item).
            let item_declaration_text_range =
                analysis_utils::ast_item_declaration_range(&match adt.clone() {
                    ast::Adt::Enum(it) => ast::Item::Enum(it),
                    ast::Adt::Struct(it) => ast::Item::Struct(it),
                    ast::Adt::Union(it) => ast::Item::Union(it),
                })
                .unwrap_or(adt.syntax().text_range());

            Diagnostic {
                message: format!(
                    "`ErrorCode` associated type must implement the {trait_paths_display} trait{}.",
                    if unimplemented_traits.len() == 1 {
                        ""
                    } else {
                        "s"
                    }
                ),
                range: item_declaration_text_range,
                severity: Severity::Error,
                quickfixes: Some(vec![Action {
                    label: format!(
                        "Derive {trait_paths_display} trait implementation{}{}.",
                        if unimplemented_traits.len() == 1 {
                            ""
                        } else {
                            "s"
                        },
                        adt.name()
                            .as_ref()
                            .map(ToString::to_string)
                            .map(|name| format!(" for `{name}`"))
                            .unwrap_or_default()
                    ),
                    kind: ActionKind::QuickFix,
                    range: item_declaration_text_range,
                    edits: vec![TextEdit::new(
                        insert_text,
                        insert_range,
                        Some(insert_snippet),
                    )],
                }]),
            }
        })
    })
}

// Returns an error diagnostic for every usage of `Self::ErrorCode` in the chain extension or its defined methods.
//
// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L1269-L1274>.
fn ensure_no_self_error_code_usage(
    results: &mut Vec<Diagnostic>,
    chain_extension: &ChainExtension,
) {
    let error_code_path_option = error_code_adt(chain_extension)
        .as_ref()
        .and_then(resolution::item_path);
    for self_error_code_path in chain_extension.syntax().descendants().filter_map(|node| {
        ast::Path::cast(node).filter(|path| path.to_string().replace(' ', "") == "Self::ErrorCode")
    }) {
        let range = self_error_code_path.syntax().text_range();
        results.push(Diagnostic {
            message: "Due to technical limitations, it is not possible to refer to \
            the `ErrorCode` associated type using `Self::ErrorCode` \
            anywhere within the chain extension and its defined methods. \
            \nUse the error code type directly instead."
                .to_string(),
            range,
            severity: Severity::Error,
            quickfixes: error_code_path_option.as_ref().map(|error_code_path| {
                vec![Action {
                    label: format!("Replace `{self_error_code_path}` with `{error_code_path}`"),
                    kind: ActionKind::QuickFix,
                    range,
                    edits: vec![TextEdit::replace(error_code_path.clone(), range)],
                }]
            }),
        });
    }
}

// Returns the error code ADT (struct, enum or union) (if any).
fn error_code_adt(chain_extension: &ChainExtension) -> Option<ast::Adt> {
    ink_analyzer_ir::resolve_item(
        &resolution::path_from_type(&chain_extension.error_code()?.ty()?)?,
        chain_extension.syntax(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{parse_first_ink_entity_of_type, verify_actions};
    use quote::quote;
    use test_utils::{quote_as_pretty_string, TestResultAction, TestResultTextRange};

    // `ErrorCode` type definition.
    macro_rules! error_code_type_defs {
        () => {
            [
                // Fully qualified paths.
                (
                    quote! {},
                    quote! {
                        #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
                    },
                    quote! {}
                ),
                (
                    quote! {},
                    quote! {
                        #[derive(ink::scale::Encode, ink::scale::Decode, ink::scale_info::TypeInfo)]
                    },
                    quote! {}
                ),
                (
                    quote! {},
                    quote! {
                        #[derive(parity_scale_codec::Encode, parity_scale_codec::Decode, scale_info::TypeInfo)]
                    },
                    quote! {}
                ),
                // Scoped paths.
                (
                    quote! {
                        use scale::{Encode, Decode};
                        use scale_info::TypeInfo;
                    },
                    quote! {
                        #[derive(Encode, Decode, TypeInfo)]
                    },
                    quote! {}
                ),
                (
                    quote! {
                        use ink::scale::{Encode, Decode};
                        use ink::scale_info::TypeInfo;
                    },
                    quote! {
                        #[derive(Encode, Decode, TypeInfo)]
                    },
                    quote! {}
                ),
                (
                    quote! {
                        use ink::scale;
                        use ink::scale_info;
                    },
                    quote! {
                        #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
                    },
                    quote! {}
                ),
                (
                    quote! {
                        use parity_scale_codec::{Encode, Decode};
                        use scale_info::TypeInfo;
                    },
                    quote! {
                        #[derive(Encode, Decode, TypeInfo)]
                    },
                    quote! {}
                ),
                // Conditional attributes.
                (
                    quote! {},
                    quote! {
                        #[cfg_attr(feature = "std", derive(scale::Encode, scale::Decode, scale_info::TypeInfo))]
                    },
                    quote! {}
                ),
                (
                    quote! {},
                    quote! {
                        #[derive(scale::Encode, scale::Decode)]
                        #[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
                    },
                    quote! {}
                ),
            ]
            .into_iter()
            .chain(
                // Custom implementations.
                [
                    // Simple paths.
                    (
                        quote! {},
                        quote! { scale:: },
                        quote! { scale_info:: }
                    ),
                    (
                        quote! {},
                        quote! { ink::scale:: },
                        quote! { ink::scale_info:: }
                    ),
                    (
                        quote! {},
                        quote! { parity_scale_codec:: },
                        quote! { scale_info:: }
                    ),
                    // Scoped paths.
                    (
                        quote! {
                            use scale::{Encode, Decode, Error, Input};
                            use scale_info::{TypeInfo, Type};
                        },
                        quote! {},
                        quote! {}
                    ),
                    (
                        quote! {
                            use ink::scale::{Encode, Decode, Error, Input};
                            use ink::scale_info::{TypeInfo, Type};
                        },
                        quote! {},
                        quote! {}
                    ),
                    (
                        quote! {
                            use parity_scale_codec::{Encode, Decode, Error, Input};
                            use scale_info::{TypeInfo, Type};
                        },
                        quote! {},
                        quote! {}
                    ),
                ]
                .into_iter()
                .map(|(imports, scale_qualifier, scale_info_qualifier)| {
                    (
                        imports,
                        quote! {},
                        quote! {
                            impl #scale_qualifier Encode for MyErrorCode {}

                            impl #scale_qualifier Decode for MyErrorCode {
                                fn decode<I: #scale_qualifier Input>(input: &mut I) -> Result<Self,#scale_qualifier Error> {
                                    // --snip--
                                }
                            }

                            impl #scale_info_qualifier TypeInfo for MyErrorCode {
                                fn type_info() -> #scale_info_qualifier Type {
                                    // --snip--
                                }
                            }
                        }
                    )
                })
            )
            .map(|(imports, attrs, impls)| {
                quote! {
                    #imports

                    #attrs
                    pub enum MyErrorCode {
                        InvalidKey,
                        CannotWriteToKey,
                        CannotReadFromKey,
                    }

                    impl ink::env::chain_extension::FromStatusCode for MyErrorCode {
                        fn from_status_code(status_code: u32) -> Result<(), Self> {
                            match status_code {
                                0 => Ok(()),
                                1 => Err(Self::InvalidKey),
                                2 => Err(Self::CannotWriteToKey),
                                3 => Err(Self::CannotReadFromKey),
                                _ => panic!("encountered unknown status code"),
                            }
                        }
                    }

                    #impls
                }
            })
        };
    }

    macro_rules! default_error_code_type_def {
        () => {
            error_code_type_defs!().next().unwrap()
        };
    }

    macro_rules! valid_error_codes {
        () => {
            error_code_type_defs!()
                .flat_map(|type_def| {
                    [
                        quote! { type ErrorCode = MyErrorCode; },
                        quote! { type ErrorCode = crate::MyErrorCode; },
                        quote! { type ErrorCode = self::MyErrorCode; },
                    ]
                    .into_iter()
                    .map(move |type_alias| (type_def.clone(), type_alias))
                })
                .flat_map(|(type_def, type_alias)| {
                    [
                        // No extensions.
                        quote! {},
                        // Extension variations.
                        quote! {
                            #[ink(extension=1)]
                            fn my_extension();

                            #[ink(extension=2)]
                            fn my_extension4(a: i32) -> bool;
                        },
                    ]
                    .into_iter()
                    .map(move |extensions| {
                        quote_as_pretty_string! {
                            #[ink::chain_extension]
                            pub trait my_chain_extension {
                                #type_alias

                                #extensions
                            }

                            #type_def
                        }
                    })
                })
        };
    }

    #[test]
    fn resolvable_error_code_works() {
        for code in valid_error_codes!() {
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_resolvable(&chain_extension);
            assert!(result.is_none(), "code: {code}");
        }
    }

    #[test]
    fn unresolvable_error_code_fails() {
        for (type_def, type_alias, expected_quickfixes) in [
            // Wrong path to existing `ErrorCode` type.
            (
                default_error_code_type_def!(),
                quote! { type ErrorCode = super::MyErrorCode; },
                vec![TestResultAction {
                    label: "Replace `super::MyErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("<-super::MyErrorCode"),
                        end_pat: Some("super::MyErrorCode"),
                    }],
                }],
            ),
            // Non-existent `ErrorCode` type (with no local `ErrorCode` type definition).
            (quote! {}, quote! { type ErrorCode = MyErrorCode; }, vec![]),
            (
                quote! {},
                quote! { type ErrorCode = crate::MyErrorCode; },
                vec![],
            ),
            (
                quote! {},
                quote! { type ErrorCode = self::MyErrorCode; },
                vec![],
            ),
            // Non-existent `ErrorCode` type (with local `ErrorCode` type definition).
            (
                default_error_code_type_def!(),
                quote! { type ErrorCode = NoErrorCode; },
                vec![TestResultAction {
                    label: "Replace `NoErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("<-NoErrorCode"),
                        end_pat: Some("NoErrorCode"),
                    }],
                }],
            ),
            (
                default_error_code_type_def!(),
                quote! { type ErrorCode = super::NoErrorCode; },
                vec![TestResultAction {
                    label: "Replace `super::NoErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("<-super::NoErrorCode"),
                        end_pat: Some("super::NoErrorCode"),
                    }],
                }],
            ),
            // Non-path `ErrorCode` type.
            (quote! {}, quote! { type ErrorCode = (); }, vec![]),
            (
                default_error_code_type_def!(),
                quote! { type ErrorCode = (); },
                vec![TestResultAction {
                    label: "Replace `()`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("type ErrorCode = "),
                        end_pat: Some("type ErrorCode = ()"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait my_chain_extension {
                    #type_alias

                    // --snip--
                }

                #type_def
            };
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_resolvable(&chain_extension);

            // Verifies diagnostics.
            assert!(result.is_some(), "code: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "code: {code}"
            );
            // Verifies quickfixes.
            let empty = Vec::new();
            let quickfixes = result
                .as_ref()
                .unwrap()
                .quickfixes
                .as_ref()
                .unwrap_or(&empty);
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn impl_from_status_code_works() {
        for code in valid_error_codes!() {
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_impl_from_status_code(&chain_extension);
            assert!(result.is_none(), "code: {code}");
        }
    }

    #[test]
    fn no_impl_from_status_code_fails() {
        for (type_def, type_alias, expected_quickfixes) in [(
            quote! {
                #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
                pub struct MyErrorCode;
            },
            quote! { type ErrorCode = crate::MyErrorCode; },
            vec![TestResultAction {
                label: "Add `ink::env::chain_extension::FromStatusCode`",
                edits: vec![TestResultTextRange {
                    text: "impl ink::env::chain_extension::FromStatusCode for ",
                    start_pat: Some("pub struct MyErrorCode;"),
                    end_pat: Some("pub struct MyErrorCode;"),
                }],
            }],
        )] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait my_chain_extension {
                    #type_alias

                    // --snip--
                }

                #type_def
            };
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_impl_from_status_code(&chain_extension);

            // Verifies diagnostics.
            assert!(result.is_some(), "code: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "code: {code}"
            );
            // Verifies quickfixes.
            let empty = Vec::new();
            let quickfixes = result
                .as_ref()
                .unwrap()
                .quickfixes
                .as_ref()
                .unwrap_or(&empty);
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn impl_scale_codec_traits_works() {
        for code in valid_error_codes!() {
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_impl_scale_codec_traits(&chain_extension);
            assert!(result.is_none(), "code: {code}");
        }
    }

    #[test]
    fn missing_impl_scale_codec_traits_fails() {
        for (attrs, expected_quickfixes) in [
            (
                quote! {},
                vec![TestResultAction {
                    label: "Derive `scale::Encode`, `scale::Decode`, `scale_info::TypeInfo`",
                    edits: vec![TestResultTextRange {
                        text: "scale::Encode, scale::Decode, scale_info::TypeInfo",
                        start_pat: Some("<-pub enum MyErrorCode {"),
                        end_pat: Some("<-pub enum MyErrorCode {"),
                    }],
                }],
            ),
            (
                quote! {
                    #[derive(scale::Encode, scale::Decode)]
                },
                vec![TestResultAction {
                    label: "Derive `scale_info::TypeInfo`",
                    edits: vec![TestResultTextRange {
                        text: "scale_info::TypeInfo",
                        start_pat: Some("<-#[derive(scale::Encode, scale::Decode)]"),
                        end_pat: Some("#[derive(scale::Encode, scale::Decode)]"),
                    }],
                }],
            ),
            (
                quote! {
                    #[derive(scale::Encode, scale_info::TypeInfo)]
                },
                vec![TestResultAction {
                    label: "Derive `scale::Decode`",
                    edits: vec![TestResultTextRange {
                        text: "scale::Decode",
                        start_pat: Some("<-#[derive(scale::Encode, scale_info::TypeInfo)]"),
                        end_pat: Some("#[derive(scale::Encode, scale_info::TypeInfo)]"),
                    }],
                }],
            ),
            (
                quote! {
                    #[derive(scale::Decode, scale_info::TypeInfo)]
                },
                vec![TestResultAction {
                    label: "Derive `scale::Encode`",
                    edits: vec![TestResultTextRange {
                        text: "scale::Encode",
                        start_pat: Some("<-#[derive(scale::Decode, scale_info::TypeInfo)]"),
                        end_pat: Some("#[derive(scale::Decode, scale_info::TypeInfo)]"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait my_chain_extension {
                    type ErrorCode = crate::MyErrorCode;

                    // --snip--
                }

                #attrs
                pub enum MyErrorCode {
                    InvalidKey,
                    CannotWriteToKey,
                    CannotReadFromKey,
                }

                impl ink::env::chain_extension::FromStatusCode for MyErrorCode {
                    fn from_status_code(status_code: u32) -> Result<(), Self> {
                        match status_code {
                            0 => Ok(()),
                            1 => Err(Self::InvalidKey),
                            2 => Err(Self::CannotWriteToKey),
                            3 => Err(Self::CannotReadFromKey),
                            _ => panic!("encountered unknown status code"),
                        }
                    }
                }
            };
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let result = ensure_impl_scale_codec_traits(&chain_extension);

            // Verifies diagnostics.
            assert!(result.is_some(), "code: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "code: {code}"
            );
            // Verifies quickfixes.
            let empty = Vec::new();
            let quickfixes = result
                .as_ref()
                .unwrap()
                .quickfixes
                .as_ref()
                .unwrap_or(&empty);
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn no_self_error_code_usage_works() {
        for code in valid_error_codes!() {
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let mut results = Vec::new();
            ensure_no_self_error_code_usage(&mut results, &chain_extension);
            assert!(results.is_empty(), "code: {code}");
        }
    }

    #[test]
    fn self_error_code_usage_fails() {
        for (extensions, expected_quickfixes) in [
            (
                quote! {
                    #[ink(extension=1)]
                    fn my_extension(a: Self::ErrorCode);
                },
                vec![TestResultAction {
                    label: "Replace `Self::ErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("<-Self::ErrorCode"),
                        end_pat: Some("Self::ErrorCode"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(extension=1)]
                    fn my_extension() -> Self::ErrorCode;
                },
                vec![TestResultAction {
                    label: "Replace `Self::ErrorCode`",
                    edits: vec![TestResultTextRange {
                        text: "crate::MyErrorCode",
                        start_pat: Some("<-Self::ErrorCode"),
                        end_pat: Some("Self::ErrorCode"),
                    }],
                }],
            ),
        ] {
            let type_def = default_error_code_type_def!();
            let code = quote_as_pretty_string! {
                #[ink::chain_extension]
                pub trait my_chain_extension {
                    type ErrorCode = crate::MyErrorCode;

                    #extensions
                }

                #type_def
            };
            let chain_extension = parse_first_ink_entity_of_type(&code);

            let mut results = Vec::new();
            ensure_no_self_error_code_usage(&mut results, &chain_extension);

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "code: {code}");
            assert_eq!(results[0].severity, Severity::Error, "code: {code}");
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }
}
