//! ink! test diagnostics.

use ink_analyzer_ir::InkTest;

use super::utils;
use crate::Diagnostic;

/// Runs all ink! test diagnostics.
///
/// The entry point for finding ink! test semantic rules is the ink_test module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L34-L44>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27-L30>.
pub fn diagnostics(ink_test: &InkTest) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(ink_test));

    // Ensure ink! test is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27>.
    if let Some(diagnostic) = utils::ensure_fn(ink_test) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! test has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(ink_test, "test"),
    );

    results
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Severity;
    use ink_analyzer_ir::{
        quote_as_str, FromInkAttribute, IRItem, InkAttributeKind, InkFile, InkMacroKind,
    };
    use quote::quote;

    fn parse_first_ink_test(code: &str) -> InkTest {
        InkTest::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::Test))
                .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn fn_works() {
        let ink_test = parse_first_ink_test(quote_as_str! {
            #[ink::test]
            fn it_works() {
            }
        });

        let result = utils::ensure_fn(&ink_test);
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
            let ink_test = parse_first_ink_test(quote_as_str! {
                #[ink::test]
                #code
            });

            let result = utils::ensure_fn(&ink_test);
            assert!(result.is_some(), "ink test: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "ink test: {}",
                code
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

        let results = utils::ensure_no_ink_descendants(&ink_test, "test");
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let ink_test = parse_first_ink_test(quote_as_str! {
            #[ink::test]
            fn it_works() {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let results = utils::ensure_no_ink_descendants(&ink_test, "test");
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
}
