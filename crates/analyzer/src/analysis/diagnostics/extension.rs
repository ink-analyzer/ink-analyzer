//! ink! extension diagnostics.

use ink_analyzer_ir::{Extension, InkFn};

use super::utils;
use crate::Diagnostic;

const EXTENSION_SCOPE_NAME: &str = "extension";

/// Runs all ink! extension diagnostics.
///
/// The entry point for finding ink! extension semantic rules is the chain_extension module of the ink_ir crate.
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
    use crate::Severity;
    use ink_analyzer_ir::{FromInkAttribute, InkArgKind, InkAttributeKind, InkEntity, InkFile};
    use quote::quote;
    use test_utils::quote_as_str;

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
            assert!(results.is_empty(), "extension: {}", code);
        }
    }

    #[test]
    fn invalid_method_fails() {
        for code in [
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L720-L729>.
            quote! {
                fn my_extension<T>();
            },
            // Const fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L665-L674>.
            quote! {
                const fn my_extension();
            },
            // Async fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L676-L685>.
            quote! {
                async fn my_extension();
            },
            // Unsafe fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L687-L696>.
            quote! {
                unsafe fn my_extension();
            },
            // Explicit ABI fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            quote! {
                extern "C" fn my_extension();
            },
            // Variadic fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/chain_extension.rs#L698-L707>.
            quote! {
                fn my_extension(...);
            },
        ] {
            let extension = parse_first_extension(quote_as_str! {
                #[ink(extension=1)]
                #code
            });

            let mut results = Vec::new();
            utils::ensure_method_invariants(
                &mut results,
                extension.fn_item().unwrap(),
                EXTENSION_SCOPE_NAME,
            );
            assert_eq!(results.len(), 1, "extension: {}", code);
            assert_eq!(results[0].severity, Severity::Error, "extension: {}", code);
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
            assert!(result.is_none(), "extension: {}", code);
        }
    }

    #[test]
    // Ref: <>.
    fn self_receiver_fails() {
        for code in [
            quote! {
                fn my_extension(self);
            },
            quote! {
                fn my_extension(mut self);
            },
            quote! {
                fn my_extension(&self);
            },
            quote! {
                fn my_extension(&mut self);
            },
        ] {
            let extension = parse_first_extension(quote_as_str! {
                #[ink(extension=1)]
                #code
            });

            let result =
                utils::ensure_no_self_receiver(extension.fn_item().unwrap(), EXTENSION_SCOPE_NAME);
            assert!(result.is_some(), "extension: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "extension: {}",
                code
            );
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
            assert!(results.is_empty(), "extension: {}", code);
        }
    }

    #[test]
    fn ink_descendants_fails() {
        let extension = parse_first_extension(quote_as_str! {
            #[ink(extension=1)]
            fn my_extension() {
                #[ink(event)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

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
    }

    #[test]
    fn compound_diagnostic_works() {
        for code in valid_extensions!() {
            let extension = parse_first_extension(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &extension);
            assert!(results.is_empty(), "extension: {}", code);
        }
    }
}
