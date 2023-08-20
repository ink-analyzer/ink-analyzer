//! ink! e2e test diagnostics.

use ink_analyzer_ir::InkE2ETest;

use super::utils;
use crate::Diagnostic;

const E2E_TEST_SCOPE_NAME: &str = "e2e test";

/// Runs all ink! test diagnostics.
///
/// The entry point for finding ink! e2e test semantic rules is the `ir` module of the `ink_e2e_macro` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L37-L48>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, ink_test: &InkE2ETest) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, ink_test);

    // Ensures that ink! e2e test is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/ir.rs#L42>.
    if let Some(diagnostic) = utils::ensure_fn(ink_test, E2E_TEST_SCOPE_NAME) {
        results.push(diagnostic);
    }

    // Ensures that ink! e2e test has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, ink_test, E2E_TEST_SCOPE_NAME);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{FromInkAttribute, InkAttributeKind, InkFile, InkMacroKind, IsInkEntity};
    use quote::quote;
    use test_utils::quote_as_str;

    fn parse_first_ink_e2e_test(code: &str) -> InkE2ETest {
        InkE2ETest::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::E2ETest))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn fn_works() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        let result = utils::ensure_fn(&ink_e2e_test, E2E_TEST_SCOPE_NAME);
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
            let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
                #[ink_e2e::test]
                #code
            });

            let result = utils::ensure_fn(&ink_e2e_test, E2E_TEST_SCOPE_NAME);
            assert!(result.is_some(), "ink e2e test: {code}");
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "ink e2e test: {code}"
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

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &ink_e2e_test, E2E_TEST_SCOPE_NAME);
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &ink_e2e_test, E2E_TEST_SCOPE_NAME);
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
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.2.1/crates/e2e/macro/src/lib.rs#L46-L85>.
    fn compound_diagnostic_works() {
        let ink_e2e_test = parse_first_ink_e2e_test(quote_as_str! {
            type E2EResult<T> = std::result::Result<T, Box<dyn std::error::Error>>;

            #[ink_e2e::test]
            async fn it_works(mut client: ::ink_e2e::Client<C,E>) -> E2EResult<()> {
            }
        });

        let mut results = Vec::new();
        diagnostics(&mut results, &ink_e2e_test);
        assert!(results.is_empty());
    }
}
