//! ink! extension diagnostics.

use ink_analyzer_ir::{Extension, IsInkFn};

use super::utils;
use crate::Diagnostic;

const EXTENSION_SCOPE_NAME: &str = "extension";

/// Runs all ink! extension diagnostics.
///
/// The entry point for finding ink! extension semantic rules is the `chain_extension` module of the `ink_ir` crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L467-L500>.
pub fn diagnostics(results: &mut Vec<Diagnostic>, extension: &Extension) {
    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::run_generic_diagnostics(results, extension);

    // Ensures that ink! extension is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L473>.
    if let Some(diagnostic) = utils::ensure_fn(extension, EXTENSION_SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(fn_item) = extension.fn_item() {
        // Ensures that ink! extension `fn` item satisfies all common invariants of method-based ink! entities,
        // see `utils::ensure_method_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L395-L465>.
        utils::ensure_method_invariants(results, fn_item, EXTENSION_SCOPE_NAME);

        // Ensures that ink! extension `fn` item has no self receiver, see `utils::ensure_no_self_receiver` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L488-L493>.
        if let Some(diagnostic) = utils::ensure_no_self_receiver(fn_item, EXTENSION_SCOPE_NAME) {
            results.push(diagnostic);
        }
    }

    // Ensures that ink! extension has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::ensure_no_ink_descendants(results, extension, EXTENSION_SCOPE_NAME);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::verify_actions;
    use crate::Severity;
    use ink_analyzer_ir::{FromInkAttribute, InkArgKind, InkAttributeKind, InkFile, IsInkEntity};
    use quote::quote;
    use test_utils::{quote_as_pretty_string, quote_as_str, TestResultAction, TestResultTextRange};

    fn parse_first_extension(code: &str) -> Extension {
        Extension::cast(
            InkFile::parse(code)
                .tree()
                .ink_attrs_in_scope()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Extension))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! extensions used for positive(`works`) tests for ink! extension verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L878-L887>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L926-L939>.
    macro_rules! valid_extensions {
        () => {
            [
                // no input + no output
                quote! {
                    fn my_extension();
                },
                // single input only
                quote! {
                    fn my_extension(a: i32);
                },
                // single output only
                quote! {
                    fn my_extension() -> bool;
                },
                // single input + single output
                quote! {
                    fn my_extension(a: i32) -> bool;
                },
                // single input + tuple output
                quote! {
                    fn my_extension(a: i32) -> (i32, u64, bool);
                },
                // many inputs + output
                quote! {
                    fn my_extension(a: i32, b: u64, c: [u8; 32]) -> bool;
                },
                // many inputs + tuple output
                quote! {
                    fn my_extension(a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool);
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink(extension=1)]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink(extension=1, handle_status=false)]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_method_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_method_invariants(
                &mut results,
                extension.fn_item().unwrap(),
                EXTENSION_SCOPE_NAME,
            );
            assert!(results.is_empty(), "extension: {code}");
        }
    }

    #[test]
    fn invalid_method_fails() {
        for (code, expected_quickfixes) in [
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
            (
                quote! {
                    fn my_extension<T>();
                },
                vec![TestResultAction {
                    label: "Remove generic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-<T>"),
                        end_pat: Some("<T>"),
                    }],
                }],
            ),
            // Const fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
            (
                quote! {
                    const fn my_extension();
                },
                vec![TestResultAction {
                    label: "Remove `const`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const fn"),
                        end_pat: Some("const "),
                    }],
                }],
            ),
            // Async fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
            (
                quote! {
                    async fn my_extension();
                },
                vec![TestResultAction {
                    label: "Remove `async`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-async"),
                        end_pat: Some("async "),
                    }],
                }],
            ),
            // Unsafe fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
            (
                quote! {
                    unsafe fn my_extension();
                },
                vec![TestResultAction {
                    label: "Remove `unsafe`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-unsafe"),
                        end_pat: Some("unsafe "),
                    }],
                }],
            ),
            // Explicit ABI fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            (
                quote! {
                    extern "C" fn my_extension();
                },
                vec![TestResultAction {
                    label: "Remove explicit ABI",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(r#"<-extern "C""#),
                        end_pat: Some(r#"extern "C" "#),
                    }],
                }],
            ),
            // Variadic fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            (
                quote! {
                    fn my_extension(...);
                },
                vec![TestResultAction {
                    label: "un-variadic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-..."),
                        end_pat: Some("..."),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(extension=1)]
                #code
            };
            let extension = parse_first_extension(&code);

            let mut results = Vec::new();
            utils::ensure_method_invariants(
                &mut results,
                extension.fn_item().unwrap(),
                EXTENSION_SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "extension: {code}");
            assert_eq!(results[0].severity, Severity::Error, "extension: {code}");
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn no_self_receiver_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let result =
                utils::ensure_no_self_receiver(extension.fn_item().unwrap(), EXTENSION_SCOPE_NAME);
            assert!(result.is_none(), "extension: {code}");
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L153-L155>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.3.0/crates/ink/ir/src/ir/chain_extension.rs#L812-L859>.
    fn self_receiver_fails() {
        for (code, start_pat, end_pat) in [
            (
                quote! {
                    fn my_extension(self);
                },
                "<-self",
                "self",
            ),
            (
                quote! {
                    fn my_extension(mut self);
                },
                "<-mut self",
                "mut self",
            ),
            (
                quote! {
                    fn my_extension(&self);
                },
                "<-&self",
                "&self",
            ),
            (
                quote! {
                    fn my_extension(&mut self);
                },
                "<-&mut self",
                "&mut self",
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink(extension=1)]
                #code
            };
            let extension = parse_first_extension(&code);

            let result =
                utils::ensure_no_self_receiver(extension.fn_item().unwrap(), EXTENSION_SCOPE_NAME);

            // Verifies diagnostics.
            assert!(result.is_some(), "extension: {code}");
            assert_eq!(
                result.as_ref().unwrap().severity,
                Severity::Error,
                "extension: {code}"
            );
            // Verifies quickfixes.
            let expected_quickfixes = vec![
                // Removes self receiver.
                TestResultAction {
                    label: "self receiver",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some(start_pat),
                        end_pat: Some(end_pat),
                    }],
                },
            ];
            let quickfixes = result.as_ref().unwrap().quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes);
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            utils::ensure_no_ink_descendants(&mut results, &extension, EXTENSION_SCOPE_NAME);
            assert!(results.is_empty(), "extension: {code}");
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let code = quote_as_pretty_string! {
            #[ink(extension=1)]
            fn my_extension() {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        };
        let extension = parse_first_extension(&code);

        let mut results = Vec::new();
        utils::ensure_no_ink_descendants(&mut results, &extension, EXTENSION_SCOPE_NAME);

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
    fn compound_diagnostic_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &extension);
            assert!(results.is_empty(), "extension: {code}");
        }
    }
}
