//! ink! test diagnostics.

use ink_analyzer_ir::{AsInkFn, FromInkAttribute, FromSyntax, InkTest};

use super::utils;
use crate::{Diagnostic, Severity};

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

    // Ensure ink! test is applied to an `fn` item., see `ensure_fn` doc.
    if let Some(diagnostic) = ensure_fn(ink_test) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! test has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(ink_test, "test"),
    );

    results
}

/// Ensure ink! test is applied to an `fn` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/ink_test.rs#L27>.
fn ensure_fn(ink_test: &InkTest) -> Option<Diagnostic> {
    ink_test.fn_item().is_none().then_some(Diagnostic {
        message: format!(
            "`{}` can only be applied to an `fn` item.",
            ink_test.ink_attr().syntax()
        ),
        range: ink_test.syntax().text_range(),
        severity: Severity::Error,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, IRItem, InkAttributeKind, InkFile, InkMacroKind};
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

        let result = ensure_fn(&ink_test);
        assert!(result.is_none());
    }

    #[test]
    fn non_fn_fails() {
        for code in [
            quote! {
                struct Flipper {
                }
            },
            quote! {
                enum Flipper {
                }
            },
            quote! {
                mod flipper;
            },
            quote! {
                trait Trait {
                }
            },
        ] {
            let ink_test = parse_first_ink_test(quote_as_str! {
                #[ink::test]
                #code
            });

            let result = ensure_fn(&ink_test);
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
                self.value = !self.value;
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
                struct Flip {
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
