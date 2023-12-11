//! ink! chain extension `ErrorCode` diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::{ast, ChainExtension, InkEntity};

use super::utils;
use crate::codegen::snippets::{FROM_STATUS_CODE_IMPL_PLAIN, FROM_STATUS_CODE_IMPL_SNIPPET};
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, TextEdit};

const INK_ENV_CHAIN_EXTENSION_QUALIFIERS: [&str; 2] =
    ["ink::env::chain_extension", "ink_env::chain_extension"];

/// Runs all ink! chain extension `ErrorCode` diagnostics.
pub fn diagnostics(results: &mut Vec<Diagnostic>, item: &ChainExtension) {
    // Ensures that ink! chain extension `ErrorCode` type can be resolved, see `ensure_resolvable` doc.
    if let Some(diagnostic) = ensure_resolvable(item) {
        results.push(diagnostic);
    }

    // Ensures that ink! chain extension `ErrorCode` type satisfies the required trait bounds,
    // see `ensure_impl_from_status_code` doc.
    if let Some(diagnostic) = ensure_impl_from_status_code(item) {
        results.push(diagnostic);
    }
}

// Ensures that the ink! chain extension `ErrorCode` type can be resolved to an ADT item (i.e. struct, enum or union).
fn ensure_resolvable(item: &ChainExtension) -> Option<Diagnostic> {
    // Only continue if there's an `ErrorCode` type.
    let error_code_type = item.error_code()?.ty()?;
    let error_code_path = resolution::path_from_type(&error_code_type)?;
    let error_code_option: Option<ast::Adt> =
        ink_analyzer_ir::resolve_item(&error_code_path, item.syntax());

    match error_code_option {
        // Handles no resolved `ErrorCode` type.
        None => {
            // Determines text range for the argument value.
            let range = error_code_type.syntax().text_range();

            Some(Diagnostic {
                message: "ink! chain extension `ErrorCode` associated type should be a path to a custom type \
                that implements the `ink::env::chain_extension::FromStatusCode` trait.".to_string(),
                range,
                severity: Severity::Error,
                quickfixes: resolution::resolve_or_find_adt_by_external_trait_impl(
                    &error_code_path,
                    item.syntax(),
                    "FromStatusCode",
                    &INK_ENV_CHAIN_EXTENSION_QUALIFIERS,
                )
                    .as_ref()
                    .and_then(resolution::item_path)
                    .map(|candidate_path| {
                        // Suggests a resolved path.
                        vec![Action {
                            label: format!(
                                "Replace `{error_code_path}` associated type with `{candidate_path}`"
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
fn ensure_impl_from_status_code(item: &ChainExtension) -> Option<Diagnostic> {
    // Only continue if there's a named `ErrorCode` type.
    let error_code_type = item.error_code()?.ty()?;
    let error_code_path = resolution::path_from_type(&error_code_type)?;
    let adt: ast::Adt = ink_analyzer_ir::resolve_item(&error_code_path, item.syntax())?;
    let name = adt.name()?.to_string();

    utils::ensure_external_trait_impl(
        &adt,
        (
            "FromStatusCode",
            &INK_ENV_CHAIN_EXTENSION_QUALIFIERS,
            &item.syntax().ancestors().last()?,
        ),
        "`ErrorCode` associated types must implement the `ink::env::chain_extension::FromStatusCode` trait".to_string(),
        format!("Add `ink::env::chain_extension::FromStatusCode` implementation for {name}"),
        FROM_STATUS_CODE_IMPL_PLAIN.replace("MyErrorCode", &name),
        Some(FROM_STATUS_CODE_IMPL_SNIPPET.replace("MyErrorCode", &name)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{parse_first_ink_entity_of_type, verify_actions};
    use quote::quote;
    use test_utils::{quote_as_pretty_string, TestResultAction, TestResultTextRange};

    // `ErrorCode` type definition.
    macro_rules! error_code_type_def {
        () => {
            quote! {
                #[derive(scale::Encode, scale::Decode, scale_info::TypeInfo)]
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
            }
        };
    }

    macro_rules! valid_error_codes {
        () => {
            [
                (
                    error_code_type_def!(),
                    quote! { type ErrorCode = MyErrorCode; },
                ),
                (
                    error_code_type_def!(),
                    quote! { type ErrorCode = crate::MyErrorCode; },
                ),
                (
                    error_code_type_def!(),
                    quote! { type ErrorCode = self::MyErrorCode; },
                ),
            ]
            .iter()
            .map(|(type_def, type_alias)| {
                quote_as_pretty_string! {
                    #type_def

                    #[ink::chain_extension]
                    pub trait my_chain_extension {
                        #type_alias

                        // --snip--
                    }
                }
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
                error_code_type_def!(),
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
            // Non-existent environment (with local custom environment definition).
            (
                error_code_type_def!(),
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
                error_code_type_def!(),
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
        ] {
            let code = quote_as_pretty_string! {
                #type_def

                #[ink::chain_extension]
                pub trait my_chain_extension {
                    #type_alias

                    // --snip--
                }
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
                #type_def

                #[ink::chain_extension]
                pub trait my_chain_extension {
                    #type_alias

                    // --snip--
                }
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
}
