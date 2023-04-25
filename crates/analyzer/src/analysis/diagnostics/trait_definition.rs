//! ink! trait definition diagnostics.

use ink_analyzer_ir::ast::AstNode;
use ink_analyzer_ir::syntax::SyntaxKind;
use ink_analyzer_ir::{
    FromInkAttribute, FromSyntax, InkArgKind, InkAttributeKind, Message, TraitDefinition,
};

use super::{message, utils};
use crate::{Diagnostic, Severity};

/// Runs all ink! trait definition diagnostics.
///
/// The entry point for finding ink! trait definition semantic rules is the trait_def module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/mod.rs#L42-L49>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L64-L84>.
pub fn diagnostics(trait_definition: &TraitDefinition) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut utils::run_generic_diagnostics(trait_definition),
    );

    // Ensure ink! trait definition is a `trait` item that satisfies all common invariants of trait-based ink! entities,
    // see `utils::ensure_trait_invariants` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L108-L148>.
    utils::append_diagnostics(
        &mut results,
        &mut utils::ensure_trait_invariants(trait_definition),
    );

    // Ensure ink! trait definition is a `trait` item whose associated items satisfy all invariants,
    // see `ensure_trait_item_invariants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_trait_item_invariants(trait_definition),
    );

    // Ensure at least one ink! message, see `ensure_contains_message` doc.
    if let Some(diagnostic) = ensure_contains_message(trait_definition) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // see `ensure_valid_quasi_direct_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_valid_quasi_direct_ink_descendants(trait_definition),
    );

    results
}

/// Ensure ink! trait definition is a `trait` item whose associated items satisfy all invariants.
///
/// See reference below for details about checked invariants.
/// This utility also runs `message::diagnostics` on trait methods with a ink! message attribute.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L150-L208>.
fn ensure_trait_item_invariants(trait_definition: &TraitDefinition) -> Vec<Diagnostic> {
    utils::ensure_trait_item_invariants(
        trait_definition,
        "trait definition",
        |fn_item| {
            let mut results = Vec::new();
            // All trait methods should be ink! messages.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L210-L288>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L298-L322>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L290-L296>.
            if let Some(message_item) = ink_analyzer_ir::ink_attrs(fn_item.syntax())
                .into_iter()
                .find_map(Message::cast)
            {
                // Run ink! message diagnostics, see `message::diagnostics` doc.
                results.append(&mut message::diagnostics(&message_item));
            } else {
                results.push(Diagnostic {
                    message: "All ink! trait definition methods must be ink! messages.".to_string(),
                    range: fn_item.syntax().text_range(),
                    severity: Severity::Error,
                })
            }

            // Wildcard selectors are not supported.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/trait_item.rs#L80-L101>.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L304>.
            results.append(&mut ink_analyzer_ir::ink_attrs(fn_item.syntax()).iter().flat_map(|attr| {
                attr.args().iter().filter_map(|arg| {
                    if let Some(value) = arg.value() {
                        if matches!(value.kind(), SyntaxKind::UNDERSCORE | SyntaxKind::UNDERSCORE_EXPR) {
                            return Some(Diagnostic {
                                message:
                                "Wildcard selectors (i.e `selector=_`) on ink! trait definition methods are not supported. \
                                They're only supported on inherent ink! messages and constructors."
                                    .to_string(),
                                range: arg.text_range(),
                                severity: Severity::Error,
                            });
                        }
                    }
                    None
                })
            }).collect());

            results
        },
        |type_alias| {
            vec![Diagnostic {
                message: "Associated types in ink! trait definitions are not yet supported."
                    .to_string(),
                range: type_alias.syntax().text_range(),
                severity: Severity::Error,
            }]
        },
    )
}

/// Ensure at least one ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L73-L79>.
fn ensure_contains_message(trait_definition: &TraitDefinition) -> Option<Diagnostic> {
    utils::ensure_at_least_one_item(
        trait_definition.messages(),
        Diagnostic {
            message: "At least one ink! message has to be defined for an ink! trait definition."
                .to_string(),
            range: trait_definition.syntax().text_range(),
            severity: Severity::Error,
        },
    )
}

/// Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L163-L164>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L290-L296>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/trait_item.rs#L85-L99>.
fn ensure_valid_quasi_direct_ink_descendants(
    trait_definition: &TraitDefinition,
) -> Vec<Diagnostic> {
    utils::ensure_valid_quasi_direct_ink_descendants(trait_definition, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(InkArgKind::Message)
                | InkAttributeKind::Arg(InkArgKind::Payable)
                | InkAttributeKind::Arg(InkArgKind::Default)
                | InkAttributeKind::Arg(InkArgKind::Selector)
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, IRItem, InkFile, InkMacroKind};
    use quote::{format_ident, quote};

    fn parse_first_trait_definition(code: &str) -> TraitDefinition {
        TraitDefinition::cast(
            InkFile::parse(code)
                .ink_attrs_in_scope()
                .into_iter()
                .find(|attr| *attr.kind() == InkAttributeKind::Macro(InkMacroKind::TraitDefinition))
                .unwrap(),
        )
        .unwrap()
    }

    // List of valid minimal ink! trait definitions used for positive(`works`) tests for ink! trait definition verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L329-L334>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L360-L365>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L375-L380>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L391-L404>.
    macro_rules! valid_traits {
        () => {
            [
                // Simple.
                quote! {
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message(&self);
                        #[ink(message)]
                        fn my_message_mut(&mut self);
                    }
                },
                // Selectors.
                quote! {
                    pub trait MyTrait {
                        #[ink(message, selector = 0xDEADBEEF)]
                        fn my_message(&self);
                        #[ink(message, selector = 0xC0FEFEED)]
                        fn my_message_mut(&mut self);
                    }
                },
                // Payable.
                quote! {
                    pub trait MyTrait {
                        #[ink(message, payable)]
                        fn my_message(&self);
                        #[ink(message, payable)]
                        fn my_message_mut(&mut self);
                    }
                },
                // Compound.
                quote! {
                    pub trait MyTrait {
                        #[ink(message)]
                        fn my_message_1(&self);
                        #[ink(message, payable)]
                        fn my_message_2(&self);
                        #[ink(message, payable, selector = 0xDEADBEEF)]
                        fn my_message_3(&self);
                        #[ink(message)]
                        fn my_message_mut_1(&mut self);
                        #[ink(message, payable)]
                        fn my_message_mut_2(&mut self);
                        #[ink(message, payable, selector = 0xC0DEBEEF)]
                        fn my_message_mut_3(&mut self);
                    }
                },
            ]
        };
    }

    #[test]
    fn valid_trait_properties_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                #code
            });

            let results = utils::ensure_trait_invariants(&trait_definition);
            assert!(results.is_empty(), "trait definition: {}", code);
        }
    }

    #[test]
    fn invalid_trait_properties_fails() {
        for code in [
            // Visibility.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L48-L58>.
            quote! {
                trait MyTrait {}
            },
            quote! {
                crate trait MyTrait {}
            },
            quote! {
                pub(crate) trait MyTrait {}
            },
            quote! {
                pub(self) trait MyTrait {}
            },
            quote! {
                pub(super) trait MyTrait {}
            },
            quote! {
                pub(in my::path) trait MyTrait {}
            },
            // Unsafe.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L32-L38>.
            quote! {
                pub unsafe trait MyTrait {}
            },
            // Auto.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L40-L46>.
            quote! {
                pub auto trait MyTrait {}
            },
            // Generic.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L60-L66>.
            quote! {
                pub trait MyTrait<T> {}
            },
            // Supertrait.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L68-L74>.
            quote! {
                pub trait MyTrait: SuperTrait {}
            },
        ] {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                #code
            });

            let results = utils::ensure_trait_invariants(&trait_definition);
            assert_eq!(results.len(), 1, "trait definition: {}", code);
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "trait definition: {}",
                code
            );
        }
    }

    #[test]
    fn valid_trait_items_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                #code
            });

            let results = ensure_trait_item_invariants(&trait_definition);
            assert!(results.is_empty(), "trait definition: {}", code);
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        // NOTE: This test only checks that a method has a `message` attribute
        for code in [
            // Const.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L76-L84>.
            quote! {
                pub trait MyTrait {
                    const T: i32;
                }
            },
            // Type.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L86-L94>.
            quote! {
                pub trait MyTrait {
                    type Type;
                }
            },
            // Macro.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L96-L104>.
            quote! {
                pub trait MyTrait {
                    my_macro_call!();
                }
            },
            // Non-flagged method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L106-L126>.
            quote! {
                pub trait MyTrait {
                    fn non_flagged_1(&self);
                }
            },
            quote! {
                pub trait MyTrait {
                    fn non_flagged_2(&mut self);
                }
            },
            quote! {
                pub trait MyTrait {
                    fn non_flagged_3() -> Self;
                }
            },
            // Default implementation.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L128-L144>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn default_implemented(&self) {}
                }
            },
            // Const method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L146-L162>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    const fn const_message(&self);
                }
            },
            // Async method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L164-L180>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    async fn async_message(&self);
                }
            },
            // Unsafe method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L182-L198>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    unsafe fn unsafe_message(&self);
                }
            },
            // Explicit ABI.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L200-L216>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    extern fn extern_message(&self);
                }
            },
            // Variadic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L218-L234>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn variadic_message(&self, ...);
                }
            },
            // Generic method.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L236-L252>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn const_message<T>(&self);
                }
            },
            // Unsupported ink! attribute.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L254-L270>.
            quote! {
                pub trait MyTrait {
                    #[ink(constructor)]
                    fn my_constructor() -> Self;
                }
            },
            quote! {
                pub trait MyTrait {
                    #[ink(storage)]
                    fn unsupported_ink_attribute(&self);
                }
            },
            quote! {
                pub trait MyTrait {
                    #[ink(unknown)]
                    fn unknown_ink_attribute(&self);
                }
            },
            // Invalid message.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L272-L295>.
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn does_not_return_self();
                }
            },
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn does_not_return_self(self: &Self);
                }
            },
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    fn does_not_return_self(self);
                }
            },
            // Wildcard selectors.
            quote! {
                pub trait MyTrait {
                    #[ink(message, selector=_)]
                    fn has_wildcard_selector(&self);
                }
            },
            quote! {
                pub trait MyTrait {
                    #[ink(message)]
                    #[ink(selector=_)]
                    fn has_wildcard_selector(&self);
                }
            },
        ] {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                #code
            });

            let results = ensure_trait_item_invariants(&trait_definition);
            assert_eq!(results.len(), 1, "trait definition: {}", code);
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "trait definition: {}",
                code
            );
        }
    }

    #[test]
    fn one_message_works() {
        let trait_definition = parse_first_trait_definition(quote_as_str! {
            #[ink::trait_definition]
            pub trait MyTrait {
                #[ink(message)]
                fn my_message(&self) {
                }
            }
        });

        let result = ensure_contains_message(&trait_definition);
        assert!(result.is_none());
    }

    #[test]
    fn multiple_messages_works() {
        // Tests snippets with btn 2 and 5 messages.
        for idx in 2..=5 {
            // Creates multiple messages.
            let messages = (1..=idx).map(|i| {
                let name = format_ident!("my_message{i}");
                quote! {
                    #[ink(message)]
                    fn #name(&self) {
                    }
                }
            });

            // Creates contract with multiple messages.
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                pub trait MyTrait {
                    #( #messages )*
                }
            });

            let result = ensure_contains_message(&trait_definition);
            assert!(result.is_none());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L706-L722>.
    fn missing_message_fails() {
        let trait_definition = parse_first_trait_definition(quote_as_str! {
            #[ink::trait_definition]
            pub trait MyTrait {
            }
        });

        let result = ensure_contains_message(&trait_definition);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_traits!() {
            let trait_definition = parse_first_trait_definition(quote_as_str! {
                #[ink::trait_definition]
                #code
            });

            let results = ensure_valid_quasi_direct_ink_descendants(&trait_definition);
            assert!(results.is_empty());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/tests.rs#L254-L270>.
    fn invalid_quasi_direct_descendant_fails() {
        let trait_definition = parse_first_trait_definition(quote_as_str! {
            #[ink::trait_definition]
            pub trait MyTrait {
                #[ink(constructor)]
                fn my_constructor() -> Self;

                #[ink(event)]
                fn unsupported_ink_attribute(&self);

                #[ink(unknown)]
                fn unknown_ink_attribute(&self);
            }
        });

        let results = ensure_valid_quasi_direct_ink_descendants(&trait_definition);
        // 1 diagnostics for `constructor` `event` and `unknown`.
        assert_eq!(results.len(), 3);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            3
        );
    }
}
