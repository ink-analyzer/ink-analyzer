//! ink! contract reference diagnostics.

use ink_analyzer_ir::{ast, ContractRef, InkAttributeKind, InkEntity, InkMacroKind, IsInkTrait};

use super::{common, message, trait_definition::ensure_trait_interface_item_invariants};
use crate::analysis::actions::entity as entity_actions;
use crate::analysis::utils;
use crate::{ActionKind, Diagnostic, Severity, Version};

const SCOPE_NAME: &str = "contract reference";

/// Runs all ink! contract reference diagnostics.
///
/// ink! contract references currently have the same semantic rules as ink! contract references,
/// however, this is expected to change in the future.
///
/// Ref: <https://github.com/use-ink/ink/pull/2648>
pub fn diagnostics(results: &mut Vec<Diagnostic>, contract_ref: &ContractRef, version: Version) {
    // `contract_ref` attributes is not supported in ink! <= 6.x
    // Note: unsupported warnings are handled in `common::validate_entity_attributes`.
    if version.is_lte_v5() {
        return;
    }

    // Runs generic diagnostics, see `utils::run_generic_diagnostics` doc.
    common::run_generic_diagnostics(results, contract_ref, version);

    // Ensures that ink! contract reference is a `trait` item, see `utils::ensure_trait` doc.
    if let Some(diagnostic) = common::ensure_trait(contract_ref, SCOPE_NAME) {
        results.push(diagnostic);
    }

    if let Some(trait_item) = contract_ref.trait_item() {
        // Ensures that ink! contract reference `trait` item satisfies all common invariants of trait-based ink! entities,
        // see `utils::ensure_trait_invariants` doc.
        common::ensure_trait_invariants(results, trait_item, SCOPE_NAME);

        // Ensures that ink! contract reference `trait` item's associated items satisfy all invariants,
        // see `ensure_trait_item_invariants` doc.
        ensure_trait_interface_item_invariants(results, trait_item, SCOPE_NAME, version);
    }

    // Runs ink! message diagnostics, see `message::diagnostics` doc.
    for item in contract_ref.messages() {
        message::diagnostics(results, item, version);
    }

    // Ensures that at least one ink! message, see `ensure_contains_message` doc.
    if let Some(diagnostic) = ensure_contains_message(contract_ref) {
        results.push(diagnostic);
    }

    // Ensures that only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // see `ensure_valid_quasi_direct_ink_descendants` doc.
    ensure_valid_quasi_direct_ink_descendants(results, contract_ref, version);
}

/// Ensures that at least one ink! message.
fn ensure_contains_message(contract_ref: &ContractRef) -> Option<Diagnostic> {
    // Gets the declaration range for the item.
    let range = contract_ref
        .trait_item()
        .and_then(|it| utils::ast_item_declaration_range(&ast::Item::Trait(it.clone())))
        .unwrap_or(contract_ref.syntax().text_range());
    common::ensure_at_least_one_item(
        contract_ref.messages(),
        Diagnostic {
            message: "At least one ink! message must be defined for an ink! contract reference."
                .to_owned(),
            range,
            severity: Severity::Error,
            quickfixes: entity_actions::add_message_to_contract_ref(
                contract_ref,
                ActionKind::QuickFix,
                None,
            )
            .map(|action| vec![action]),
        },
    )
}

/// Ensures that only valid quasi-direct ink! attribute descendants (i.e. ink! descendants without any ink! ancestors).
fn ensure_valid_quasi_direct_ink_descendants(
    results: &mut Vec<Diagnostic>,
    contract_ref: &ContractRef,
    version: Version,
) {
    common::ensure_valid_quasi_direct_ink_descendants_by_kind(
        results,
        contract_ref,
        InkAttributeKind::Macro(InkMacroKind::ContractRef),
        version,
        SCOPE_NAME,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use ink_analyzer_ir::syntax::{TextRange, TextSize};
    use quote::{format_ident, quote};
    use test_utils::{
        parse_offset_at, quote_as_pretty_string, quote_as_str, TestResultAction,
        TestResultTextRange,
    };

    fn parse_first_contract_ref(code: &str) -> ContractRef {
        parse_first_ink_entity_of_type(code)
    }

    // List of valid minimal ink! contract references used for positive(`works`) tests for ink! contract reference verifying utilities.
    macro_rules! valid_contract_refs {
        () => {
            [
                // Simple.
                quote! {
                    #[ink(message)]
                    fn my_message(&self);

                    #[ink(message)]
                    fn my_message_mut(&mut self);
                },
                // Selectors.
                quote! {
                    #[ink(message, selector = 0xDEADBEEF)]
                    fn my_message(&self);

                    #[ink(message, selector = 0xC0FEFEED)]
                    fn my_message_mut(&mut self);
                },
                // Payable.
                quote! {
                    #[ink(message, payable)]
                    fn my_message_mut(&mut self);
                },
                // Compound.
                quote! {
                    #[ink(message)]
                    fn my_message_1(&self);

                    #[ink(message, selector = 0xDEADBEEF)]
                    fn my_message_2(&self);

                    #[ink(message, name="myMessage3")]
                    fn my_message_3(&self);

                    #[ink(message)]
                    fn my_message_mut_1(&mut self);

                    #[ink(message, payable)]
                    fn my_message_mut_2(&mut self);

                    #[ink(message, payable, selector = 0xC0DEBEEF, name="myMessageMut3")]
                    fn my_message_mut_3(&mut self);
                },
            ]
            .iter()
            .flat_map(|messages| {
                [
                    // Simple.
                    quote! {
                        #[ink::contract_ref]
                        pub trait Callee {
                            #messages
                        }
                    },
                    // ABI.
                    quote! {
                        #[ink::contract_ref(abi="ink")]
                        pub trait Callee {
                            #messages
                        }
                    },
                    quote! {
                        #[ink::contract_ref(abi="sol")]
                        pub trait Callee {
                            #messages
                        }
                    },
                    // Keep Attr.
                    quote! {
                        #[ink::contract_ref(env=ink::env::DefaultEnvironment)]
                        pub trait Callee {
                            #messages
                        }
                    },
                    // Compound.
                    quote! {
                        #[ink::contract_ref(abi="sol", env=ink::env::DefaultEnvironment)]
                        pub trait Callee {
                            #messages
                        }
                    },
                ]
            })
        };
    }

    #[test]
    fn valid_trait_properties_works() {
        for code in valid_contract_refs!() {
            let contract_ref = parse_first_contract_ref(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            common::ensure_trait_invariants(
                &mut results,
                contract_ref.trait_item().unwrap(),
                SCOPE_NAME,
            );
            assert!(results.is_empty(), "contract reference: {code}");
        }
    }

    #[test]
    fn invalid_trait_properties_fails() {
        for (code, expected_quickfixes) in [
            // Visibility.
            (
                quote! {
                    trait Callee {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-trait Callee"),
                        end_pat: Some("<-trait Callee"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(crate) trait Callee {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(crate)"),
                        end_pat: Some("pub(crate)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(self) trait Callee {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(self)"),
                        end_pat: Some("pub(self)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(super) trait Callee {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(super)"),
                        end_pat: Some("pub(super)"),
                    }],
                }],
            ),
            (
                quote! {
                    pub(in my::path) trait Callee {}
                },
                vec![TestResultAction {
                    label: "`pub`",
                    edits: vec![TestResultTextRange {
                        text: "pub",
                        start_pat: Some("<-pub(in my::path)"),
                        end_pat: Some("pub(in my::path)"),
                    }],
                }],
            ),
            // Unsafe.
            (
                quote! {
                    pub unsafe trait Callee {}
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
            // Auto.
            (
                quote! {
                    pub auto trait Callee {}
                },
                vec![TestResultAction {
                    label: "Remove `auto`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-auto"),
                        end_pat: Some("auto "),
                    }],
                }],
            ),
            // Generic.
            (
                quote! {
                    pub trait Callee<T> {}
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
            // Supertrait.
            (
                quote! {
                    pub trait Callee: SuperTrait {}
                },
                vec![TestResultAction {
                    label: "Remove type",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-: SuperTrait"),
                        end_pat: Some(": SuperTrait"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::contract_ref]
                #code
            };
            let contract_ref = parse_first_contract_ref(&code);

            let mut results = Vec::new();
            common::ensure_trait_invariants(
                &mut results,
                contract_ref.trait_item().unwrap(),
                SCOPE_NAME,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "contract reference: {code}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "contract reference: {code}"
            );
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn valid_trait_items_works() {
        for code in valid_contract_refs!() {
            let contract_ref = parse_first_contract_ref(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_trait_interface_item_invariants(
                &mut results,
                contract_ref.trait_item().unwrap(),
                SCOPE_NAME,
                Version::V6,
            );
            assert!(results.is_empty(), "contract reference: {code}");
        }
    }

    #[test]
    fn invalid_trait_items_fails() {
        for (items, expected_quickfixes) in [
            // Const.
            (
                quote! {
                    const T: i32;
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const"),
                        end_pat: Some("i32;"),
                    }],
                }],
            ),
            // Type.
            (
                quote! {
                    type Type;
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-type"),
                        end_pat: Some("Type;"),
                    }],
                }],
            ),
            // Macro.
            (
                quote! {
                    my_macro_call!();
                },
                vec![TestResultAction {
                    label: "Remove macro",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-my_macro_call"),
                        end_pat: Some("my_macro_call!();"),
                    }],
                }],
            ),
            // Non-flagged method.
            (
                quote! {
                    fn non_flagged(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-fn"),
                        end_pat: Some("<-fn"),
                    }],
                }],
            ),
            (
                quote! {
                    fn non_flagged_mut(&mut self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![TestResultTextRange {
                        text: "message",
                        start_pat: Some("<-fn"),
                        end_pat: Some("<-fn"),
                    }],
                }],
            ),
            // Default implementation.
            (
                quote! {
                    #[ink(message)]
                    fn default_implemented(&self) {}
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-{}"),
                        end_pat: Some("{}"),
                    }],
                }],
            ),
            // Const method.
            (
                quote! {
                    #[ink(message)]
                    const fn const_message(&self);
                },
                vec![TestResultAction {
                    label: "Remove `const`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-const"),
                        end_pat: Some("const "),
                    }],
                }],
            ),
            // Async method.
            (
                quote! {
                    #[ink(message)]
                    async fn async_message(&self);
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
            // Unsafe method.
            (
                quote! {
                    #[ink(message)]
                    unsafe fn unsafe_message(&self);
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
            // Explicit ABI.
            (
                quote! {
                    #[ink(message)]
                    extern fn extern_message(&self);
                },
                vec![TestResultAction {
                    label: "Remove explicit ABI",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-extern"),
                        end_pat: Some("extern "),
                    }],
                }],
            ),
            // Variadic method.
            (
                quote! {
                    #[ink(message)]
                    fn variadic_message(&self, ...);
                },
                vec![TestResultAction {
                    label: "un-variadic",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, ..."),
                        end_pat: Some("..."),
                    }],
                }],
            ),
            // Generic method.
            (
                quote! {
                    #[ink(message)]
                    fn generic_message<T>(&self);
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
            // Unsupported ink! attribute.
            (
                quote! {
                    #[ink(constructor)]
                    fn my_constructor() -> Self;
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("<-#[ink(constructor)]"),
                        },
                        // Remove ink! constructor attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(constructor)]"),
                            end_pat: Some("#[ink(constructor)]"),
                        },
                    ],
                }],
            ),
            (
                quote! {
                    #[ink(storage)]
                    fn unsupported_method(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("<-#[ink(storage)]"),
                        },
                        // Remove ink! storage attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(storage)]"),
                            end_pat: Some("#[ink(storage)]"),
                        },
                    ],
                }],
            ),
            (
                quote! {
                    #[ink(unknown)]
                    fn unknown_method(&self);
                },
                vec![TestResultAction {
                    label: "Add ink! message",
                    edits: vec![
                        // Add ink! message attribute.
                        TestResultTextRange {
                            text: "message",
                            start_pat: Some("<-#[ink(unknown)]"),
                            end_pat: Some("<-#[ink(unknown)]"),
                        },
                        // Remove unknown ink! attribute.
                        TestResultTextRange {
                            text: "",
                            start_pat: Some("<-#[ink(unknown)]"),
                            end_pat: Some("#[ink(unknown)]"),
                        },
                    ],
                }],
            ),
            // Invalid message.
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver();
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver(self: &Self);
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            (
                quote! {
                    #[ink(message)]
                    fn no_self_ref_receiver(self);
                },
                vec![
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                    TestResultAction {
                        label: "Add",
                        edits: vec![TestResultTextRange {
                            text: "&mut self",
                            start_pat: Some("no_self_ref_receiver("),
                            end_pat: Some("no_self_ref_receiver("),
                        }],
                    },
                ],
            ),
            // Wildcard selectors.
            (
                quote! {
                    #[ink(message, selector = _)]
                    fn has_wildcard_selector(&self);
                },
                vec![TestResultAction {
                    label: "Remove wildcard",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-, selector = _"),
                        end_pat: Some("selector = _"),
                    }],
                }],
            ),
            (
                quote! {
                    #[ink(message)]
                    #[ink(selector = _)]
                    fn has_wildcard_selector(&self);
                },
                vec![TestResultAction {
                    label: "Remove",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(selector = _)]"),
                        end_pat: Some("#[ink(selector = _)]"),
                    }],
                }],
            ),
        ] {
            let code = quote_as_pretty_string! {
                #[ink::contract_ref]
                pub trait Callee {
                    #items
                }
            };
            let contract_ref = parse_first_contract_ref(&code);

            let mut results = Vec::new();
            ensure_trait_interface_item_invariants(
                &mut results,
                contract_ref.trait_item().unwrap(),
                SCOPE_NAME,
                Version::V6,
            );

            // Verifies diagnostics.
            assert_eq!(results.len(), 1, "contract reference: {items}");
            assert_eq!(
                results[0].severity,
                Severity::Error,
                "contract reference: {items}"
            );
            // Verifies quickfixes.
            verify_actions(
                &code,
                results[0].quickfixes.as_ref().unwrap(),
                &expected_quickfixes,
            );
        }
    }

    #[test]
    fn one_message_works() {
        let contract_ref = parse_first_contract_ref(quote_as_str! {
            #[ink::contract_ref]
            pub trait Callee {
                #[ink(message)]
                fn my_message(&self) {
                }
            }
        });

        let result = ensure_contains_message(&contract_ref);
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
            let contract_ref = parse_first_contract_ref(quote_as_str! {
                #[ink::contract_ref]
                pub trait Callee {
                    #( #messages )*
                }
            });

            let result = ensure_contains_message(&contract_ref);
            assert!(result.is_none());
        }
    }

    #[test]
    fn missing_message_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract_ref]
            pub trait Callee {
            }
        };
        let contract_ref = parse_first_contract_ref(&code);

        let result = ensure_contains_message(&contract_ref);

        // Verifies diagnostics.
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().severity, Severity::Error);
        // Verifies quickfixes.
        assert!(result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0]
            .label
            .contains("Add ink! message"));
        let offset = TextSize::from(parse_offset_at(&code, Some("{")).unwrap() as u32);
        assert_eq!(
            result.as_ref().unwrap().quickfixes.as_ref().unwrap()[0].edits[0].range,
            TextRange::new(offset, offset)
        );
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_contract_refs!() {
            let contract_ref = parse_first_contract_ref(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            ensure_valid_quasi_direct_ink_descendants(&mut results, &contract_ref, Version::V6);
            assert!(results.is_empty());
        }
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let code = quote_as_pretty_string! {
            #[ink::contract_ref]
            pub trait Callee {
                #[ink(constructor)]
                fn my_constructor() -> Self;

                #[ink(event)]
                fn unsupported_method(&self);
            }
        };
        let contract_ref = parse_first_contract_ref(&code);

        let mut results = Vec::new();
        ensure_valid_quasi_direct_ink_descendants(&mut results, &contract_ref, Version::V6);
        // 1 diagnostic each for `constructor` and `event`.
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
                    label: "Remove `#[ink(constructor)]`",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("#[ink(constructor)]"),
                    }],
                },
                TestResultAction {
                    label: "Remove item",
                    edits: vec![TestResultTextRange {
                        text: "",
                        start_pat: Some("<-#[ink(constructor)]"),
                        end_pat: Some("fn my_constructor() -> Self;"),
                    }],
                },
            ],
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
                        end_pat: Some("fn unsupported_method(&self);"),
                    }],
                },
            ],
        ];
        for (idx, item) in results.iter().enumerate() {
            let quickfixes = item.quickfixes.as_ref().unwrap();
            verify_actions(&code, quickfixes, &expected_quickfixes[idx]);
        }
    }

    #[test]
    fn compound_diagnostic_works() {
        for code in valid_contract_refs!() {
            let contract_ref = parse_first_contract_ref(quote_as_str! {
                #code
            });

            let mut results = Vec::new();
            diagnostics(&mut results, &contract_ref, Version::V6);
            assert!(results.is_empty(), "contract reference: {code}");
        }
    }
}
