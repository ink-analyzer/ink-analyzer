//! ink! test diagnostics.

use ink_analyzer_ir::{InkAttributeKind, InkMacroKind, InkTest};

use super::common;
use crate::{Diagnostic, Version};

const SCOPE_NAME: &str = "test";
const ATTR_KIND: InkAttributeKind = InkAttributeKind::Macro(InkMacroKind::Test);

/// Runs all ink! test diagnostics.
///
/// The entry point for finding ink! test semantic rules is the `ink_test` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L34-L44>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27-L30>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, ink_test: &InkTest, version: Version) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, ink_test, version);

    // Ensures that ink! test is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27>.
    if let Some(diagnostic) = common::ensure_fn(ink_test, SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! test has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results, ink_test, ATTR_KIND, version, SCOPE_NAME,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_ink_test(code: &str) -> InkTest {
        parse_first_ink_entity_of_type(code)
    }

    #[test]
    fn fn_works() {
        let ink_test = parse_first_ink_test(quote_as_str! {
            #[ink::test]
            fn it_works() {
            }
        });

        let result = common::ensure_fn(&ink_test, SCOPE_NAME);
        assert!(result.is_none());
    }

    #[test]
    fn non_fn_fails() {
        for code in [
            quote! {
                struct MyTest {
                }
            },
            quote! {
                enum MyTest {
                }
            },
            quote! {
                mod my_test;
            },
            quote! {
                trait MyTest {
                }
            },
        ] {
            let code = quote_as_pretty_string! {
                #[ink::test]
                #code
            };
            let ink_test = parse_first_ink_test(&code);

            let result = common::ensure_fn(&ink_test, SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some(), "ink test: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "ink test: {code}"
            );
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink::test]`"));
            assert!(fix.edits[0].text.is_empty());
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(parse_offset_at(&code, Some("<-#[ink::test]")).unwrap() as u32),
                    TextSize::from(parse_offset_at(&code, Some("#[ink::test]")).unwrap() as u32)
                )
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let ink_test = parse_first_ink_test(quote_as_str! {
            #[ink::test]
            fn it_works() {
            }
        });

        for version in [Version::V4, Version::V5] {
            let mut results = Vec::new();
            common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                &mut results,
                &ink_test,
                ATTR_KIND,
                version,
                SCOPE_NAME,
            );
            assert!(results.is_empty());
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink::test]
            fn it_works() {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        };
        let ink_test = parse_first_ink_test(&code);

        for version in [Version::V4, Version::V5] {
            let mut results = Vec::new();
            common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                &mut results,
                &ink_test,
                ATTR_KIND,
                version,
                SCOPE_NAME,
            );

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
            let expected_quickfixes = vec![
                vec![
                    TestResultAction {
                        label: "Remove `#[ink(event)]`",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("#[ink(event)]"),
                        }],
                    },
                    TestResultAction {
                        label: "Remove item",
                        edits: vec![TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(event)]"),
                            end_pat: Some("}"),
                        }],
                    },
                ],
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
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/macro/src/lib.rs#L824-L841>.
    fn compound_diagnostic_works() {
        for code in [
            quote_as_str! {
                // Conventional unit test that works with assertions.
                #[ink::test]
                fn test1() {
                   // test code comes here as usual
                }
            },
            quote_as_str! {
                // Conventional unit test that returns some Result.
                // The test code can make use of operator-`?`.
                #[ink::test]
                fn test2() -> Result<(), ink_env::Error> {
                    // test code that returns a Rust Result type
                }
            },
        ] {
            let ink_test = parse_first_ink_test(code);

            let mut results = Vec::new();
            diagnostics(&mut results, &ink_test, Version::V4);
            assert!(results.is_empty(), "ink test: {code}");
        }
    }
}
