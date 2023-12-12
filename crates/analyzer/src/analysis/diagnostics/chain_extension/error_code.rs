//! ink! chain extension `ErrorCode` diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasName};
use ink_analyzer_ir::{ast, ChainExtension, InkEntity};

use super::utils;
use crate::codegen::snippets::{FROM_STATUS_CODE_IMPL_PLAIN, FROM_STATUS_CODE_IMPL_SNIPPET};
use crate::{resolution, Action, ActionKind, Diagnostic, Severity, TextEdit};

const INK_ENV_CHAIN_EXTENSION_QUALIFIERS: [&str; 2] =
    ["ink::env::chain_extension", "ink_env::chain_extension"];

/// Runs all ink! chain extension `ErrorCode` diagnostics.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L957-L976>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/macro/src/lib.rs#L1269-L1274>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, chain_extension: &ChainExtension) {
    // Ensures that ink! chain extension `ErrorCode` type can be resolved, see `ensure_resolvable` doc.
    if let Some(diagnostic) = ensure_resolvable(chain_extension) {
        results.push(diagnostic);
    }

    // Ensures that ink! chain extension `ErrorCode` type satisfies the required trait bounds,
    // see `ensure_impl_from_status_code` doc.
    if let Some(diagnostic) = ensure_impl_from_status_code(chain_extension) {
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
                            "Replace `{error_code_type}` associated type with `{candidate_path}`"
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
        "`ErrorCode` associated types must implement the `ink::env::chain_extension::FromStatusCode` trait".to_string(),
        format!("Add `ink::env::chain_extension::FromStatusCode` implementation for {name}"),
        FROM_STATUS_CODE_IMPL_PLAIN.replace("MyErrorCode", &name),
        Some(FROM_STATUS_CODE_IMPL_SNIPPET.replace("MyErrorCode", &name)),
    )
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
            .into_iter()
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
            // Non-existent `ErrorCode` type (with local `ErrorCode` type definition).
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
            // Non-path `ErrorCode` type.
            (quote! {}, quote! { type ErrorCode = (); }, vec![]),
            (
                error_code_type_def!(),
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
            let type_def = error_code_type_def!();
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
