//! ink! error diagnostics.

use ink_analyzer_ir::{Error, InkEntity};

use super::common;
use crate::{Action, Diagnostic, Severity, Version};

const SCOPE_NAME: &str = "error";

/// Runs all ink! error diagnostics.
///
/// # References
///
/// - <https://github.com/use-ink/ink/blob/master/crates/ink/macro/src/lib.rs#L1641-L1676>
/// - <https://github.com/use-ink/ink/blob/v6.0.0-alpha.4/crates/ink/macro/src/error.rs>
pub fn diagnostics(results: &mut Vec<Diagnostic>, error: &Error, version: Version) {
    // `contract_ref` attributes is not supported in ink! <= 6.x
    // Note: unsupported warnings are handled in `common::validate_entity_attributes`.
    if version.is_lte_v5() {
        return;
    }

    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, error, version);

    // Ensures that ink! error is applied to an `adt` (i.e `enum`, `struct` or `union`) item,
    // see `ensure_adt` doc.
    if let Some(diagnostic) = ensure_adt(error) {
        results.push(diagnostic);
    }

    // Ensures that ink! error has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    common::ensure_no_ink_descendants(results, error, SCOPE_NAME, false);
}

/// Ensures that ink! error is an `adt` (i.e `enum`, `struct` or `union`) item.
///
/// # References
///
/// - <https://github.com/use-ink/ink/blob/master/crates/ink/macro/src/lib.rs#L1641-L1676>
/// - <https://github.com/use-ink/ink/blob/v6.0.0-alpha.4/crates/ink/macro/src/error.rs>
// FIXME: Unions shouldn't be supported in Solidity ABI compatibility mode
fn ensure_adt(error: &Error) -> Option<Diagnostic> {
    error.adt().is_none().then(|| Diagnostic {
        message: format!(
            "`{}` can only be applied to an `enum`, `struct` or `union` item.",
            error
                .ink_attr()
                .map(|attr| attr.syntax().to_string())
                .as_deref()
                .unwrap_or("#[ink::error]")
        ),
        range: error.syntax().text_range(),
        severity: Severity::Error,
        quickfixes: error
            .ink_attr()
            .map(|attr| vec![Action::remove_attribute(attr)]),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_error(code: &str) -> Error {
        parse_first_ink_entity_of_type(code)
    }

    #[test]
    fn adt_works() {
        for code in [
            quote! {
                struct Error {
                }
            },
            quote! {
                enum Error {
                }
            },
            quote! {
                union Error {
                }
            },
        ] {
            let error = parse_first_error(quote_as_str! {
                #[ink::error]
                #code
            });

            let result = ensure_adt(&error);
            assert!(result.is_none());
        }
    }

    #[test]
    fn non_adt_fails() {
        for code in [
            quote! {
                fn error() {
                }
            },
            quote! {
                mod error;
            },
            quote! {
                trait Error {
                }
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink::error]
                #code
            };
            let error = parse_first_error(&code);

            let result = ensure_adt(&error);

            // Verifies diagnostics.
            assert!(result.is_some(), "error: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "error: {code}"
            );
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink::error]`"));
            assert!(fix.edits[0].text.is_empty());
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(parse_offset_at(&code, Some("<-#[ink::error]")).unwrap() as u32),
                    TextSize::from(parse_offset_at(&code, Some("#[ink::error]")).unwrap() as u32)
                )
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let error = parse_first_error(quote_as_str! {
            #[ink::error]
            struct Error {
            }
        });

        let mut results = Vec::new();
        common::ensure_no_ink_descendants(&mut results, &error, SCOPE_NAME, false);
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink::error]
            struct Error {
                #[ink(event)]
                field_1: (u32, bool),
                #[ink(topic)]
                field_2: String,
            }
        };
        let error = parse_first_error(&code);

        let mut results = Vec::new();
        common::ensure_no_ink_descendants(&mut results, &error, SCOPE_NAME, false);
        // 2 diagnostics for `event` and `topic`.
        assert_eq!(results.len(), 2);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            2
        );
        // Verifies quickfixes.
        let expected_quickfixes = [
            vec![TestResultAction {
                label: "Remove `#[ink(event)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(event)]"),
                    end_pat: Some("#[ink(event)]"),
                }],
            }],
            vec![TestResultAction {
                label: "Remove `#[ink(topic)]`",
                edits: vec![TestResultTextRange {
                    text: "",
                    start_pat: Some("<-#[ink(topic)]"),
                    end_pat: Some("#[ink(topic)]"),
                }],
            }],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L673-L770>.
    fn compound_diagnostic_works() {
        for code in [
            quote_as_str! {
                #[ink::error]
                struct UnitError;
            },
            quote_as_str! {
                #[ink::error]
                enum MultipleErrors {
                    UnitError,
                    ErrorWithParams(bool, u8, String),
                    ErrorWithNamedParams {
                        status: bool,
                        count: u8,
                        reason: String,
                    }
                }
            },
        ] {
            let error = parse_first_error(code);

            let mut results = Vec::new();
            diagnostics(&mut results, &error, Version::V6);
            assert!(results.is_empty(), "error: {code}");
        }
    }
}
