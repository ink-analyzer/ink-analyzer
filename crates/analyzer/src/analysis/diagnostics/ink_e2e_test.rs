//! ink! e2e test diagnostics.

use ink_analyzer_ir::{InkAttributeKind, InkE2ETest, InkMacroKind};

use super::{common, environment};
use crate::{Diagnostic, Version};

const SCOPE_NAME: &str = "e2e test";
const ATTR_KIND: InkAttributeKind = InkAttributeKind::Macro(InkMacroKind::E2ETest);

/// Runs all ink! test diagnostics.
///
/// The entry point for finding ink! e2e test semantic rules is the `ir` module of the `ink_e2e_macro` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L37-L48>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, ink_e2e_test: &InkE2ETest, version: Version) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, ink_e2e_test, version);

    // Ensures that ink! e2e test is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L42>.
    if let Some(diagnostic) = common::ensure_fn(ink_e2e_test, SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! e2e test has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results,
        ink_e2e_test,
        ATTR_KIND,
        version,
        SCOPE_NAME,
    );

    // Runs ink! environment diagnostics, see `environment::diagnostics` doc.
    environment::diagnostics(results, ink_e2e_test);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use crate::Severity;
    use ink_analyzer_ir::{
        syntax::{TextRange, TextSize},
        MinorVersion,
    };
    use quote::quote;
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_ink_e2e_test(code: &str) -> InkE2ETest {
        parse_first_ink_entity_of_type(code)
    }

    #[test]
    fn fn_works() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        let result = common::ensure_fn(&ink_e2e_test, SCOPE_NAME);
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
                #[ink_e2e::test]
                #code
            };
            let ink_e2e_test = parse_first_ink_e2e_test(&code);

            let result = common::ensure_fn(&ink_e2e_test, SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some(), "ink e2e test: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "ink e2e test: {code}"
            );
            // Verifies quickfixes.
            let fix = &result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0];
            assert!(fix.label.contains("Remove `#[ink_e2e::test]`"));
            assert!(fix.edits[0].text.is_empty());
            assert_eq!(
                fix.edits[0].range,
                TextRange::new(
                    TextSize::from(
                        parse_offset_at(&code, Some("<-#[ink_e2e::test]")).unwrap() as u32
                    ),
                    TextSize::from(parse_offset_at(&code, Some("#[ink_e2e::test]")).unwrap() as u32)
                )
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        for version in [Version::Legacy, Version::V5(MinorVersion::V5_0)] {
            let mut results = Vec::new();
            common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                &mut results,
                &ink_e2e_test,
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
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        };
        let ink_e2e_test = parse_first_ink_e2e_test(&code);

        for version in [Version::Legacy, Version::V5(MinorVersion::V5_0)] {
            let mut results = Vec::new();
            common::ensure_valid_quasi_direct_ink_descendants_by_kind(
                &mut results,
                &ink_e2e_test,
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
            let expected_quickfixes = [
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
    // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L46-L85>.
    fn compound_diagnostic_works() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test(environment = crate::MyEnvironment)]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }

            #[derive(Clone)]
            pub struct MyEnvironment;

            impl ink::env::Environment for MyEnvironment {
                const MAX_EVENT_TOPICS: usize = 3;
                type AccountId = [u8; 16];
                type Balance = u128;
                type Hash = [u8; 32];
                type Timestamp = u64;
                type BlockNumber = u32;
                type ChainExtension = ::ink::env::NoChainExtension;
            }
        });

        let mut results = Vec::new();
        diagnostics(&mut results, &ink_e2e_test, Version::Legacy);
        assert!(results.is_empty());
    }
}
