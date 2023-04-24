//! ink! message diagnostics.

use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::{AsInkFn, FromInkAttribute, FromSyntax, Message};

use super::utils;
use crate::{Diagnostic, Severity};

/// Runs all ink! message diagnostics.
///
/// The entry point for finding ink! message semantic rules is the message module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L201-L216>.
pub fn diagnostics(message: &Message) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(message));

    // Ensure ink! message is a `fn` that satisfies all common invariants of externally callable ink! entities,
    // see `utils::ensure_callable_invariants` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L202>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/callable.rs#L355-L440>.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_callable_invariants(message),
    );

    // Ensure ink! message `fn` has a self reference receiver, see `ensure_receiver_is_self_ref` doc.
    if let Some(diagnostic) = ensure_receiver_is_self_ref(message) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! message does not return `Self`, see `ensure_not_return_self` doc.
    if let Some(diagnostic) = ensure_not_return_self(message) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure ink! message has no ink! descendants, see `utils::ensure_no_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_no_ink_descendants(message, "message"),
    );

    results
}

/// Ensure ink! message `fn` has a self reference receiver (i.e `&self` or `&mut self`).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L203>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L121-L150>.
fn ensure_receiver_is_self_ref(message: &Message) -> Option<Diagnostic> {
    let ink_attr = message.ink_attr();
    let mut has_self_ref_receiver = false;
    let mut marker_token = None;

    if let Some(fn_item) = message.fn_item() {
        if let Some(param_list) = fn_item.param_list() {
            if let Some(self_param) = param_list.self_param() {
                if self_param.amp_token().is_some() {
                    has_self_ref_receiver = true; // Only case that passes.
                } else {
                    marker_token = Some(self_param.syntax().to_owned());
                }
            } else {
                marker_token = param_list
                    .params()
                    .next()
                    .map(|param| param.syntax().to_owned());
            }
        }
    }

    (!has_self_ref_receiver).then_some(Diagnostic {
        message: format!(
            "An `fn` item annotated with `{}` must have a self reference receiver (i.e `&self` or `&mut self`).",
            ink_attr.syntax()
        ),
        range: marker_token.unwrap_or(message.syntax().to_owned()).text_range(),
        severity: Severity::Error,
    })
}

/// Ensure ink! message does not return `Self`.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L204>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L152-L174>.
fn ensure_not_return_self(message: &Message) -> Option<Diagnostic> {
    let ink_attr = message.ink_attr();
    let mut returns_self = false;
    let mut marker_token = None;

    if let Some(fn_item) = message.fn_item() {
        if let Some(ret_type) = fn_item.ret_type() {
            if let Some(ty) = ret_type.ty() {
                returns_self = ty.to_string() == "Self";
                if returns_self {
                    marker_token = Some(ty.syntax().to_owned());
                }
            }
        }
    }

    returns_self.then_some(Diagnostic {
        message: format!(
            "An `fn` item annotated with `{}` must not return `Self`.",
            ink_attr.syntax()
        ),
        range: marker_token
            .unwrap_or(message.syntax().to_owned())
            .text_range(),
        severity: Severity::Error,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, IRItem, InkArgKind, InkAttributeKind, InkFile};
    use quote::quote;

    fn parse_first_message(code: &str) -> Message {
        Message::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Arg(InkArgKind::Message))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! messages used for positive(`works`) tests for ink! message verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L545-L584>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L389-L412>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L341-L364>.
    macro_rules! valid_messages {
        () => {
            [
                // &self + inherited visibility
                quote! {
                    fn my_message(&self) {}
                },
                // &self + pub
                quote! {
                    pub fn my_message(&self) {}
                },
                // &mut self + inherited visibility
                quote! {
                    fn my_message(&mut self) {}
                },
                // &mut self + pub
                quote! {
                    pub fn my_message(&mut self) {}
                },
                // &self + single input + output
                quote! {
                    fn my_message(&self, a: i32) -> bool {}
                },
                // &mut self + single input + output
                quote! {
                    fn my_message(&mut self, a: i32) -> bool {}
                },
                // &self + single input + tuple output
                quote! {
                    fn my_message(&self, a: i32) -> (i32, u64, bool) {}
                },
                // &mut self + single input + tuple output
                quote! {
                    fn my_message(&mut self, a: i32) -> (i32, u64, bool) {}
                },
                // &self + many inputs + output
                quote! {
                    fn my_message(&self, a: i32, b: u64, c: [u8; 32]) -> bool {}
                },
                // &mut self + many inputs + output
                quote! {
                    fn my_message(&mut self, a: i32, b: u64, c: [u8; 32]) -> bool {}
                },
                // &self + many inputs + tuple output
                quote! {
                    fn my_message(&self, a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool) {}
                },
                // &mut self + many inputs + tuple output
                quote! {
                    fn my_message(&mut self, a: i32, b: u64, c: [u8; 32]) -> (i32, u64, bool) {}
                },
            ]
        };
    }

    #[test]
    fn valid_callable_works() {
        for code in valid_messages!() {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let results = utils::ensure_callable_invariants(&message);
            assert!(results.is_empty(), "message: {}", code);
        }
    }

    #[test]
    fn invalid_callable_fails() {
        for code in [
            // Bad visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L751-L781>.
            quote! {
                crate fn my_message(&self) {}
            },
            quote! {
                crate fn my_message(&mut self) {}
            },
            quote! {
                pub(crate) fn my_message(&self) {}
            },
            quote! {
                pub(crate) fn my_message(&mut self) {}
            },
            quote! {
                pub(self) fn my_message(&self) {}
            },
            quote! {
                pub(self) fn my_message(&mut self) {}
            },
            quote! {
                pub(super) fn my_message(&self) {}
            },
            quote! {
                pub(super) fn my_message(&mut self) {}
            },
            quote! {
                pub(in my::path) fn my_message(&self) {}
            },
            quote! {
                pub(in my::path) fn my_message(&mut self) {}
            },
            // Generic params fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L599-L622>.
            quote! {
                fn my_message<T>(&self) {}
            },
            quote! {
                pub fn my_message<T>(&self) {}
            },
            quote! {
                fn my_message<T>(&mut self) {}
            },
            quote! {
                pub fn my_message<T>(&mut self) {}
            },
            // Const fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L656-L673>.
            quote! {
                const fn my_message(&self) {}
            },
            quote! {
                const fn my_message(&mut self) {}
            },
            // Async fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L675-L692>.
            quote! {
                async fn my_message(&self) {}
            },
            quote! {
                async fn my_message(&mut self) {}
            },
            // Unsafe fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L694-L711>.
            quote! {
                unsafe fn my_message(&self) {}
            },
            quote! {
                unsafe fn my_message(&mut self) {}
            },
            // Explicit ABI fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L713-L730>.
            quote! {
                extern "C" fn my_message(&self) {}
            },
            quote! {
                extern "C" fn my_message(&mut self) {}
            },
            // Variadic fails.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L732-L749>.
            quote! {
                fn my_message(&self, ...) {}
            },
            quote! {
                fn my_message(&mut self, ...) {}
            },
        ] {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let results = utils::ensure_callable_invariants(&message);
            assert_eq!(results.len(), 1, "message: {}", code);
            assert_eq!(results[0].severity, Severity::Error, "message: {}", code);
        }
    }

    #[test]
    fn self_ref_receiver_works() {
        for code in valid_messages!() {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let result = ensure_receiver_is_self_ref(&message);
            assert!(result.is_none(), "message: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L624-L654>.
    fn non_self_ref_receiver_fails() {
        for code in [
            quote! {
                fn my_message() {}
            },
            quote! {
                fn my_message(self) {}
            },
            quote! {
                pub fn my_message(mut self) {}
            },
            quote! {
                fn my_message(this: &Self) {}
            },
            quote! {
                pub fn my_message(this: &mut Self) {}
            },
        ] {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let result = ensure_receiver_is_self_ref(&message);
            assert!(result.is_some(), "message: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "message: {}",
                code
            );
        }
    }

    #[test]
    fn non_self_return_type_works() {
        for code in valid_messages!() {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let result = ensure_not_return_self(&message);
            assert!(result.is_none(), "message: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L624-L654>.
    fn self_return_type_fails() {
        for code in [
            quote! {
                fn my_message(&self) -> Self {}
            },
            quote! {
                fn my_message(&mut self) -> Self {}
            },
            quote! {
                pub fn my_message(&self) -> Self {}
            },
            quote! {
                pub fn my_message(&mut self) -> Self {}
            },
        ] {
            let message = parse_first_message(quote_as_str! {
                #[ink(message)]
                #code
            });

            let result = ensure_not_return_self(&message);
            assert!(result.is_some(), "message: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "message: {}",
                code
            );
        }
    }

    #[test]
    fn no_ink_descendants_works() {
        let message = parse_first_message(quote_as_str! {
            #[ink(message)]
            pub fn flip(&mut self) {
            }
        });

        let results = utils::ensure_no_ink_descendants(&message, "message");
        assert!(results.is_empty());
    }

    #[test]
    fn ink_descendants_fails() {
        let message = parse_first_message(quote_as_str! {
            #[ink(message)]
            pub fn flip(&mut self) {
                #[ink(event)]
                struct Flip {
                    #[ink(topic)]
                    value: bool,
                }
            }
        });

        let results = utils::ensure_no_ink_descendants(&message, "message");
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
