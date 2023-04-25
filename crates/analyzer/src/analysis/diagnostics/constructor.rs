//! ink! constructor diagnostics.

use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::{ast, AsInkFn, Constructor};

use super::utils;
use crate::{Diagnostic, Severity};

/// Runs all ink! constructor diagnostics.
///
/// The entry point for finding ink! constructor semantic rules is the constructor module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L155-L170>.
pub fn diagnostics(constructor: &Constructor) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::run_generic_diagnostics(constructor),
    );

    // Ensure ink! constructor is an `fn` item, see `utils::ensure_fn` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L155>.
    if let Some(diagnostic) = utils::ensure_fn(constructor) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    if let Some(fn_item) = constructor.fn_item() {
        // Ensure ink! constructor `fn` item satisfies all common invariants of externally callable ink! entities,
        // see `utils::ensure_callable_invariants` doc.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L156>.
        // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
        utils::append_diagnostics(
            &mut results,
            &mut utils::ensure_callable_invariants(fn_item, "constructor"),
        );

        // Ensure ink! constructor `fn` item has no self receiver, see `ensure_no_self_receiver` doc.
        if let Some(diagnostic) = ensure_no_self_receiver(fn_item) {
            utils::push_diagnostic(&mut results, diagnostic);
        }

        // Ensure ink! constructor `fn` item has a return type, see `ensure_return_type` doc.
        if let Some(diagnostic) = ensure_return_type(fn_item) {
            utils::push_diagnostic(&mut results, diagnostic);
        }
    }

    // Ensure ink! constructor has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(constructor, "constructor"),
    );

    results
}

/// Ensure ink! constructor `fn` has no self receiver (i.e no `&self`, `&mut self`, self or mut self).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L158>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L107-L128>.
fn ensure_no_self_receiver(fn_item: &ast::Fn) -> Option<Diagnostic> {
    if let Some(param_list) = fn_item.param_list() {
        if let Some(self_param) = param_list.self_param() {
            return Some(Diagnostic {
                message: "ink! constructors must not have a self receiver (i.e no `&self`, `&mut self`, self or mut self).".to_string(),
                range: self_param.syntax().text_range(),
                severity: Severity::Error,
            });
        }
    }

    None
}

/// Ensure ink! constructor has a return type.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L157>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L91-L105>.
fn ensure_return_type(fn_item: &ast::Fn) -> Option<Diagnostic> {
    let has_returns_type = if let Some(ret_type) = fn_item.ret_type() {
        ret_type.ty().is_some()
    } else {
        false
    };

    if !has_returns_type {
        return Some(Diagnostic {
            message: "ink! constructors must have a return type.".to_string(),
            range: fn_item.syntax().text_range(),
            severity: Severity::Error,
        });
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{
        quote_as_str, FromInkAttribute, IRItem, InkArgKind, InkAttributeKind, InkFile,
    };
    use quote::quote;

    fn parse_first_constructor(code: &str) -> Constructor {
        Constructor::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Constructor))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! constructors used for positive(`works`) tests for ink! constructor verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L370-L397>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L259-L282>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L344-L359>.
    macro_rules! valid_constructors {
        () => {
            [
                // simple + inherited visibility
                quote! {
                    fn my_constructor() -> Self {}
                },
                // simple + pub
                quote! {
                    pub fn my_constructor() -> Self {}
                },
                // Result return type + inherited visibility
                quote! {
                    fn my_constructor() -> Result<Self, ()> {}
                },
                // Result return type + pub
                quote! {
                    pub fn my_constructor() -> Result<Self, ()> {}
                },
                // simple + inherited visibility + single input
                quote! {
                    fn my_constructor(a: i32) -> Self {}
                },
                // simple + pub + single input
                quote! {
                    fn my_constructor(a: i32) -> Self {}
                },
                // simple + inherited visibility + many inputs
                quote! {
                    fn my_constructor(a: i32, b: u64, c: [u8; 32]) -> Self {}
                },
                // simple + inherited visibility + many inputs
                quote! {
                    fn my_constructor(a: i32, b: u64, c: [u8; 32]) -> Self {}
                },
            ]
        };
    }

    #[test]
    fn valid_callable_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let results =
                utils::ensure_callable_invariants(constructor.fn_item().unwrap(), "constructor");
            assert!(results.is_empty(), "constructor: {}", code);
        }
    }

    #[test]
    fn invalid_callable_fails() {
        for code in [
            // Bad visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L557-L575>.
            quote! {
                crate fn my_constructor() -> Self {}
            },
            quote! {
                crate fn my_constructor() -> Self {}
            },
            quote! {
                pub(crate) fn my_constructor() -> Self {}
            },
            quote! {
                pub(crate) fn my_constructor() -> Self {}
            },
            quote! {
                pub(self) fn my_constructor() -> Self {}
            },
            quote! {
                pub(self) fn my_constructor() -> Self {}
            },
            quote! {
                pub(super) fn my_constructor() -> Self {}
            },
            quote! {
                pub(super) fn my_constructor() -> Self {}
            },
            quote! {
                pub(in my::path) fn my_constructor() -> Self {}
            },
            quote! {
                pub(in my::path) fn my_constructor() -> Self {}
            },
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L452-L467>.
            quote! {
                fn my_constructor<T>() -> Self {}
            },
            quote! {
                pub fn my_constructor<T>() -> Self {}
            },
            quote! {
                fn my_constructor<T>() -> Self {}
            },
            quote! {
                pub fn my_constructor<T>() -> Self {}
            },
            // Const fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L469-L484>.
            quote! {
                const fn my_constructor() -> Self {}
            },
            quote! {
                const fn my_constructor() -> Self {}
            },
            // Async fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L486-L501>.
            quote! {
                async fn my_constructor() -> Self {}
            },
            quote! {
                async fn my_constructor() -> Self {}
            },
            // Unsafe fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L503-L518>.
            quote! {
                unsafe fn my_constructor() -> Self {}
            },
            quote! {
                unsafe fn my_constructor() -> Self {}
            },
            // Explicit ABI fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L520-L538>.
            quote! {
                extern "C" fn my_constructor() -> Self {}
            },
            quote! {
                extern "C" fn my_constructor() -> Self {}
            },
            // Variadic fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L540-L555>.
            quote! {
                fn my_constructor(...) -> Self {}
            },
            quote! {
                fn my_constructor(...) -> Self {}
            },
        ] {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let results =
                utils::ensure_callable_invariants(constructor.fn_item().unwrap(), "constructor");
            assert_eq!(results.len(), 1, "constructor: {}", code);
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "constructor: {}",
                code
            );
        }
    }

    #[test]
    fn no_self_receiver_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let result = ensure_no_self_receiver(constructor.fn_item().unwrap());
            assert!(result.is_none(), "constructor: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L424-L450>.
    fn self_receiver_fails() {
        for code in [
            quote! {
                fn my_constructor(self) -> Self {}
            },
            quote! {
                pub fn my_constructor(self) -> Self {}
            },
            quote! {
                fn my_constructor(mut self) -> Self {}
            },
            quote! {
                pub fn my_constructor(mut self) -> Self {}
            },
            quote! {
                fn my_constructor(&self) -> Self {}
            },
            quote! {
                pub fn my_constructor(&self) -> Self {}
            },
            quote! {
                fn my_constructor(&mut self) -> Self {}
            },
            quote! {
                pub fn my_constructor(&mut self) -> Self {}
            },
        ] {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let result = ensure_no_self_receiver(constructor.fn_item().unwrap());
            assert!(result.is_some(), "constructor: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "constructor: {}",
                code
            );
        }
    }

    #[test]
    fn return_type_works() {
        for code in valid_constructors!() {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let result = ensure_return_type(constructor.fn_item().unwrap());
            assert!(result.is_none(), "constructor: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L407-L422>.
    fn missing_return_type_fails() {
        for code in [
            quote! {
                fn my_constructor() {}
            },
            quote! {
                pub fn my_constructor() {}
            },
            quote! {
                pub fn my_constructor(a: i32) {}
            },
            quote! {
                pub fn my_constructor(a: i32) {}
            },
        ] {
            let constructor = parse_first_constructor(quote_as_str! {
                #[ink(constructor)]
                #code
            });

            let result = ensure_return_type(constructor.fn_item().unwrap());
            assert!(result.is_some(), "constructor: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "constructor: {}",
                code
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let constructor = parse_first_constructor(quote_as_str! {
            #[ink(constructor)]
            pub fn new() -> Self {
            }
        });

        let results = utils::ensure_no_ink_descendants(&constructor, "constructor");
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let constructor = parse_first_constructor(quote_as_str! {
            #[ink(constructor)]
            pub fn new() -> Self {
                #[ink(event)]
                struct Flip {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let results = utils::ensure_no_ink_descendants(&constructor, "constructor");
        // 2 diagnostics for `event` and `topic`.
        assert_eq!(results.len(), 2);
        // both diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            2
        );
    }
}