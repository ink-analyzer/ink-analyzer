//! ink! contract diagnostics.

use ink_analyzer_ir::syntax::SyntaxKind;
use ink_analyzer_ir::{
    Contract, FromSyntax, IRItem, InkArgKind, InkAttribute, InkAttributeKind, InkMacroKind,
};
use std::collections::HashSet;

use super::{constructor, event, ink_impl, ink_test, message, storage, utils};
use crate::{Diagnostic, Severity};

/// Runs all ink! contract diagnostics.
///
/// The entry point for finding ink! contract semantic rules is the contract module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs#L47-L73>.
pub fn diagnostics(contract: &Contract) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(contract));

    // Ensure ink! contract is an inline `mod` item, see `ensure_inline_module` doc.
    if let Some(diagnostic) = ensure_inline_module(contract) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure exactly one ink! storage item, see `ensure_storage_quantity` doc.
    utils::append_diagnostics(&mut results, &mut ensure_storage_quantity(contract));

    // Run ink! storage diagnostics, see `storage::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .storage()
            .iter()
            .flat_map(storage::diagnostics)
            .collect(),
    );

    // Run ink! event diagnostics, see `event::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .events()
            .iter()
            .flat_map(event::diagnostics)
            .collect(),
    );

    // Run ink! impl diagnostics, see `impl_item::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .impls()
            .iter()
            .flat_map(ink_impl::diagnostics)
            .collect(),
    );

    // Ensure at least one ink! constructor, see `ensure_contains_constructor` doc.
    if let Some(diagnostic) = ensure_contains_constructor(contract) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Run ink! constructor diagnostics, see `constructor::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .constructors()
            .iter()
            .flat_map(constructor::diagnostics)
            .collect(),
    );

    // Ensure at least one ink! message, see `ensure_contains_message` doc.
    if let Some(diagnostic) = ensure_contains_message(contract) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Run ink! message diagnostics, see `message::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .messages()
            .iter()
            .flat_map(message::diagnostics)
            .collect(),
    );

    // Ensure no ink! message or constructor selectors are overlapping,
    // see `ensure_no_overlapping_selectors` doc.
    utils::append_diagnostics(&mut results, &mut ensure_no_overlapping_selectors(contract));

    // Ensure at most one wildcard selector exists among ink! messages, as well as ink! constructors,
    // see `ensure_at_most_one_wildcard_selector` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_at_most_one_wildcard_selector(contract),
    );

    // Ensure ink! messages and constructors are defined in the root of an `impl` item,
    // see `ensure_impl_parent_for_callables` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_impl_parent_for_callables(contract),
    );

    // Run ink! test diagnostics, see `ink_test::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut contract
            .tests()
            .iter()
            .flat_map(ink_test::diagnostics)
            .collect(),
    );

    // Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_valid_quasi_direct_ink_descendants(contract),
    );

    results
}

/// Ensure ink! contract attribute is applied to an inline `mod` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L301-L309>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L298>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/contract.rs#L66>.
fn ensure_inline_module(contract: &Contract) -> Option<Diagnostic> {
    let mut error = None;

    if let Some(module) = contract.module() {
        if module.item_list().is_none() {
            error = Some(
                "The content of ink! contracts `mod` items must be defined inline.".to_string(),
            );
        }
    } else {
        error = Some("ink! contracts must be inline `mod` items".to_string());
    }

    error.map(|message| Diagnostic {
        message,
        range: contract.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensure ink! storage is not missing and there are not multiple ink! storage definitions.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L328>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L328>.
fn ensure_storage_quantity(contract: &Contract) -> Vec<Diagnostic> {
    utils::ensure_exactly_one_item(
        contract.storage(),
        Diagnostic {
            message: "Missing ink! storage item.".to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
        },
        "Only one ink! storage item can be defined for an ink! contract.",
        Severity::Error,
    )
}

/// Ensure at least one ink! constructor.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L330>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L145-L165>.
fn ensure_contains_constructor(contract: &Contract) -> Option<Diagnostic> {
    utils::ensure_at_least_one_item(
        contract.constructors(),
        Diagnostic {
            message: "At least one ink! constructor must be defined for an ink! contract."
                .to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
        },
    )
}

/// Ensure at least one ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L329>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L123-L143>.
fn ensure_contains_message(contract: &Contract) -> Option<Diagnostic> {
    utils::ensure_at_least_one_item(
        contract.messages(),
        Diagnostic {
            message: "At least one ink! message must be defined for an ink! contract.".to_string(),
            range: contract.syntax().text_range(),
            severity: Severity::Error,
        },
    )
}

/// Ensure no ink! message or constructor selectors are overlapping.
///
/// Overlaps between ink! constructor and message selectors are allowed.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L331>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L167-L240>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L336-L337>.
fn ensure_no_overlapping_selectors(contract: &Contract) -> Vec<Diagnostic> {
    let constructor_attrs: Vec<InkAttribute> = contract
        .constructors()
        .iter()
        .flat_map(|item| item.ink_attrs())
        .collect();
    let message_attrs: Vec<InkAttribute> = contract
        .messages()
        .iter()
        .flat_map(|item| item.ink_attrs())
        .collect();

    [(constructor_attrs, "constructor"), (message_attrs, "message")].iter().flat_map(|(attrs, name)| {
        let mut seen_selectors: HashSet<u32> = HashSet::new();
        attrs.iter().flat_map(|attr| {
            attr.args().iter().filter_map(|arg| {
                // We only continue if the selector is a integer.
                // Bad values will be handled by the `utils::ensure_valid_attribute_arguments`,
                // while wildcards are handled by `ensure_at_most_one_wildcard_selector`.
                if *arg.kind() == InkArgKind::Selector && arg.value()?.kind() == SyntaxKind::INT_NUMBER {
                    if let Ok(arg_value) = utils::parse_u32(arg.meta().value().to_string().as_str()) {
                        if seen_selectors.get(&arg_value).is_some() {
                            return Some(Diagnostic {
                                message: format!("Selector values must be unique across all ink! {name}s in an ink! contract."),
                                range: arg.text_range(),
                                severity: Severity::Error,
                            });
                        }
                        seen_selectors.insert(arg_value);
                    }
                }
                None
            }).collect::<Vec<Diagnostic>>()
        }).collect::<Vec<Diagnostic>>()
    }).collect()
}

/// Ensure at most one wildcard selector exists among ink! messages, as well as ink! constructors.
///
/// At most one wildcard is allowed for each group
/// (i.e a single message and a single constructor each with a wildcard selector is a valid configuration).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L332>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L242-L293>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/trait_def/item/mod.rs#L336-L337>.
fn ensure_at_most_one_wildcard_selector(contract: &Contract) -> Vec<Diagnostic> {
    let constructor_attrs: Vec<InkAttribute> = contract
        .constructors()
        .iter()
        .flat_map(|item| item.ink_attrs())
        .collect();
    let message_attrs: Vec<InkAttribute> = contract
        .messages()
        .iter()
        .flat_map(|item| item.ink_attrs())
        .collect();

    [(constructor_attrs, "constructor"), (message_attrs, "message")].iter().flat_map(|(attrs, name)| {
        let mut has_seen_wildcard = false;
        attrs.iter().flat_map(|attr| {
            attr.args().iter().filter_map(|arg| {
                if *arg.kind() == InkArgKind::Selector && matches!(arg.value()?.kind(), SyntaxKind::UNDERSCORE | SyntaxKind::UNDERSCORE_EXPR) {
                    if has_seen_wildcard {
                        return Some(Diagnostic {
                            message: format!("At most one wildcard (`_`) selector can be defined across all ink! {name}s in an ink! contract."),
                            range: arg.text_range(),
                            severity: Severity::Error,
                        });
                    }
                    has_seen_wildcard = true;
                }
                None
            }).collect::<Vec<Diagnostic>>()
        }).collect::<Vec<Diagnostic>>()
    }).collect()
}

/// Ensure ink! messages and constructors are defined in the root of an `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L36-L66>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L66-L96>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/impl_item.rs#L64-L87>.
fn ensure_impl_parent_for_callables(contract: &Contract) -> Vec<Diagnostic> {
    contract
        .constructors()
        .iter()
        .filter_map(|item| utils::ensure_impl_parent(item, "constructor"))
        .chain(
            contract
                .messages()
                .iter()
                .filter_map(|item| utils::ensure_impl_parent(item, "messages")),
        )
        .collect()
}

/// Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L98-L114>.
fn ensure_valid_quasi_direct_ink_descendants(contract: &Contract) -> Vec<Diagnostic> {
    utils::ensure_valid_quasi_direct_ink_descendants(contract, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(InkArgKind::Storage)
                | InkAttributeKind::Arg(InkArgKind::Event)
                | InkAttributeKind::Arg(InkArgKind::Anonymous)
                | InkAttributeKind::Arg(InkArgKind::Impl)
                | InkAttributeKind::Arg(InkArgKind::Namespace)
                | InkAttributeKind::Arg(InkArgKind::Constructor)
                | InkAttributeKind::Arg(InkArgKind::Message)
                | InkAttributeKind::Arg(InkArgKind::Payable)
                | InkAttributeKind::Arg(InkArgKind::Default)
                | InkAttributeKind::Arg(InkArgKind::Selector)
                | InkAttributeKind::Macro(InkMacroKind::Test)
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, InkFile};
    use quote::{format_ident, quote};

    fn parse_first_contract(code: &str) -> Contract {
        InkFile::parse(code).contracts().to_owned()[0].to_owned()
    }

    // List of valid minimal ink! contracts used for positive(`works`) tests for ink! contract verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    macro_rules! valid_contracts {
        () => {
            [
                // Minimal.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        impl Minimal {
                            #[ink(constructor)]
                            pub fn new() -> Self {}

                            #[ink(message)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                // Minimal + Event + Args.
                quote! {
                    mod minimal {
                        #[ink(storage)]
                        pub struct Minimal {}

                        #[ink(event, anonymous)]
                        pub struct MinimalEvent {
                            #[ink(topic)]
                            value: i32,
                        }

                        impl Minimal {
                            #[ink(constructor, payable, default, selector=1)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                // Flipper.
                quote! {
                    mod flipper {
                        #[ink(storage)]
                        pub struct Flipper {
                            value: bool,
                        }

                        impl Default for Flipper {
                            #[ink(constructor)]
                            fn default() -> Self {
                                Self { value: false }
                            }
                        }

                        impl Flipper {
                        #[ink(message)]
                            pub fn flip(&mut self) {
                                self.value = !self.value
                            }

                            #[ink(message)]
                            pub fn get(&self) -> bool {
                                self.value
                            }
                        }
                    }
                },
                // Flipper + Event + Args.
                quote! {
                    mod flipper {
                        #[ink(storage)]
                        pub struct Flipper {
                            value: bool,
                        }

                        #[ink(event, anonymous)]
                        pub struct Flip {
                            #[ink(topic)]
                            flipped: bool,
                        }

                        impl Default for Flipper {
                            #[ink(constructor, payable, default, selector=1)]
                            fn default() -> Self {
                                Self { value: false }
                            }
                        }

                        impl Flipper {
                            #[ink(message, payable, default, selector=1)]
                            pub fn flip(&mut self) {
                                self.value = !self.value
                            }

                            #[ink(message, selector=0x2)]
                            pub fn get(&self) -> bool {
                                self.value
                            }
                        }
                    }
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #[ink::contract]
                        #code
                    },
                    // Env.
                    quote! {
                        #[ink::contract(env=my::env::Types)]
                        #code
                    },
                    // Keep Attr.
                    quote! {
                        #[ink::contract(keep_attr="foo,bar")]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink::contract(env=my::env::Types, keep_attr="foo,bar")]
                        #code
                    },
                ]
            })
        };
    }

    #[test]
    fn inline_mod_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_inline_module(&contract);
            assert!(result.is_none());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L724-L732>.
    fn out_of_line_mod_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract;
        });

        let result = ensure_inline_module(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn non_mod_fails() {
        for code in [
            quote! {
                fn my_contract() {
                }
            },
            quote! {
                struct MyContract;
            },
            quote! {
                enum MyContract {
                }
            },
            quote! {
                trait MyContract {
                }
            },
        ] {
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                #code
            });

            let result = ensure_inline_module(&contract);
            assert!(result.is_some(), "contract: {}", code);
            assert_eq!(
                result.unwrap().severity,
                Severity::Error,
                "contract: {}",
                code
            );
        }
    }

    #[test]
    fn attribute_in_mod_body_fails() {
        let contract = parse_first_contract(quote_as_str! {
            mod my_contract {
                #[ink::contract]
            }
        });

        let result = ensure_inline_module(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn one_storage_item_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink(storage)]
                pub struct MyContract {
                }
            }
        });

        let results = ensure_storage_quantity(&contract);
        assert!(results.is_empty());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L667-L686>.
    fn missing_storage_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }
        });

        let results = ensure_storage_quantity(&contract);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].severity, Severity::Error);
    }

    #[test]
    fn multiple_storage_items_fails() {
        // Tests snippets with btn 2 and 5 storage items.
        for idx in 2..=5 {
            // Creates multiple storage items.
            let storage_items = (1..=idx).map(|i| {
                let name = format_ident!("MyContract{}", i);
                quote! {
                    #[ink(storage)]
                    pub struct #name {
                    }
                }
            });

            // Creates contract with multiple storage items.
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    #( #storage_items )*
                }
            });

            let results = ensure_storage_quantity(&contract);
            // There should be `idx-1` extraneous storage items.
            assert_eq!(results.len(), idx - 1);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                idx - 1
            );
        }
    }

    #[test]
    fn one_constructor_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {
                    }
                }
            }
        });

        let result = ensure_contains_constructor(&contract);
        assert!(result.is_none());
    }

    #[test]
    fn multiple_constructors_works() {
        // Tests snippets with btn 2 and 5 constructors.
        for idx in 2..=5 {
            // Creates multiple constructors.
            let constructors = (1..=idx).map(|i| {
                let name = format_ident!("my_constructor{i}");
                quote! {
                    #[ink(constructor)]
                    pub fn #name() -> Self {
                    }
                }
            });

            // Creates contract with multiple constructors.
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    impl MyContract {
                        #( #constructors )*
                    }
                }
            });

            let result = ensure_contains_constructor(&contract);
            assert!(result.is_none());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L688-L704>.
    fn missing_constructor_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }
        });

        let result = ensure_contains_constructor(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn one_message_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(message)]
                    pub fn my_message(&mut self) {
                    }
                }
            }
        });

        let result = ensure_contains_message(&contract);
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
                    pub fn #name(&mut self) {
                    }
                }
            });

            // Creates contract with multiple messages.
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    impl MyContract {
                        #( #messages )*
                    }
                }
            });

            let result = ensure_contains_message(&contract);
            assert!(result.is_none());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L706-L722>.
    fn missing_message_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
            }
        });

        let result = ensure_contains_message(&contract);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn no_selectors_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(message)]
                    pub fn my_message(&mut self) {
                    }
                }
            }
        });

        let results = ensure_no_overlapping_selectors(&contract);
        assert!(results.is_empty());
    }

    #[test]
    fn non_overlapping_selectors_works() {
        for code in [
            // All different.
            quote! {
                #[ink(constructor, selector=1)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=2)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=3)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=4)]
                pub fn my_message2(&mut self) {
                }
            },
            // Overlaps between constructors and messages are ok.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L838-L857>.
            quote! {
                #[ink(constructor, selector=1)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=0xA)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=1)]
                pub fn my_message() {
                }

                #[ink(message, selector=0xA)]
                pub fn my_message2() {
                }
            },
        ] {
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    impl MyContract {
                        #code
                    }
                }
            });

            let results = ensure_no_overlapping_selectors(&contract);
            assert!(results.is_empty());
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L754-L780>
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L782-L808>
    fn overlapping_selectors_fails() {
        for code in [
            // Overlapping decimal.
            quote! {
                #[ink(constructor, selector=1)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=1)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=2)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=2)]
                pub fn my_message2(&mut self) {
                }
            },
            // Overlapping hexadecimal.
            quote! {
                #[ink(constructor, selector=0xA)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=0xA)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message2(&mut self) {
                }
            },
            // Overlapping detected across decimal and hex representations.
            quote! {
                #[ink(constructor, selector=10)]
                pub fn my_constructor() -> Self {
                }

                #[ink(constructor, selector=0xA)]
                pub fn my_constructor2() -> Self {
                }

                #[ink(message, selector=11)]
                pub fn my_message(&mut self) {
                }

                #[ink(message, selector=0xB)]
                pub fn my_message2(&mut self) {
                }
            },
            // TODO: Overlapping trait implementations should fail.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L810-L836>.
            /*
            quote! {
                impl first::MyTrait for MyContract {
                    #[ink(constructor)]
                    fn my_constructor() -> Self {}

                    #[ink(message)]
                    fn my_message(&self) {}
                }

                impl second::MyTrait for MyContract {
                    #[ink(constructor)]
                    fn my_constructor() -> Self {}

                    #[ink(message)]
                    fn my_message(&self) {}
                }
            },
            */
        ] {
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    impl MyContract {
                        #code
                    }
                }
            });

            let results = ensure_no_overlapping_selectors(&contract);
            // 2 errors, 1 each for constructors and messages (i.e `my_constructor2` and `my_message2` are the overlapping selectors).
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

    #[test]
    fn no_wildcard_selector_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(message)]
                    pub fn my_message(&mut self) {
                    }
                }
            }
        });

        let results = ensure_at_most_one_wildcard_selector(&contract);
        assert!(results.is_empty());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L883-L902>.
    fn one_wildcard_selector_works() {
        // At most one wildcard is allowed for each group i.e there can be messages and constructors
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor, selector=_)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(message, selector=_)]
                    pub fn my_message(&mut self) {
                    }
                }
            }
        });

        let results = ensure_at_most_one_wildcard_selector(&contract);
        assert!(results.is_empty());
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L859-L881>.
    fn multiple_wildcard_selectors_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor, selector=_)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(constructor, selector=_)]
                    pub fn my_constructor2() -> Self {
                    }

                    #[ink(message, selector=_)]
                    pub fn my_message(&mut self) {
                    }

                    #[ink(message, selector=_)]
                    pub fn my_message2(&mut self) {
                    }
                }
            }
        });

        let results = ensure_at_most_one_wildcard_selector(&contract);
        // 2 errors, 1 each for constructors and messages (i.e `my_constructor2` and `my_message2` are the extraneous wildcard selectors).
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
    fn impl_parent_for_callables_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(message)]
                    pub fn my_message() {
                    }
                }
            }
        });

        let results = ensure_impl_parent_for_callables(&contract);
        assert!(results.is_empty());
    }

    #[test]
    fn non_impl_parent_for_callables_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink(constructor)]
                pub fn my_constructor() -> Self {
                }

                #[ink(message)]
                pub fn my_message() {
                }
            }
        });

        let results = ensure_impl_parent_for_callables(&contract);

        // There should be 2 errors (i.e for the `constructor` and `message`).
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
    fn valid_quasi_direct_descendant_works() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink(storage)]
                struct MyContract {
                }

                #[ink(event)]
                #[ink(anonymous)]
                struct MyEvent {
                    #[ink(topic)]
                    value: bool,
                }

                impl MyContract {
                    #[ink(constructor)]
                    pub fn my_constructor() -> Self {
                    }

                    #[ink(message)]
                    #[ink(payable)]
                    pub fn my_message(&mut self) {
                    }
                }

                #[ink(impl)]
                impl MyContractTrait for MyContract {
                }

                #[cfg(test)]
                mod tests {
                    #[ink::test]
                    fn it_works() {
                    }
                }
            }
        });

        let results = ensure_valid_quasi_direct_ink_descendants(&contract);
        assert!(results.is_empty());
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                #[ink::trait_definition]
                trait MyTrait {
                }

                #[ink::chain_extension]
                trait MyChainExtension {
                }

                #[ink::storage_item]
                struct MyStorageItem {
                }
            }
        });

        let results = ensure_valid_quasi_direct_ink_descendants(&contract);
        // There should be 3 errors (i.e `trait_definition`, `chain_extension` and `storage_item`).
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

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    fn compound_diagnostic_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = diagnostics(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
    }
}
