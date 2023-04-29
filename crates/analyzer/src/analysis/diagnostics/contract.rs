//! ink! contract diagnostics.

use ink_analyzer_ir::syntax::SyntaxNode;
use ink_analyzer_ir::{
    Contract, FromSyntax, InkArgKind, InkAttributeKind, InkCallable, InkItem, InkMacroKind,
    Selector, SelectorArg,
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

    // Ensure ink! impls are defined in the root of the ink! contract, see `ensure_impls_in_root` doc.
    utils::append_diagnostics(&mut results, &mut ensure_impls_in_root(contract));

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

/// Returns composed selectors for a list of ink! callable entities.
fn get_composed_selectors<T>(items: &[T]) -> Vec<(Selector, SyntaxNode)>
where
    T: InkCallable,
{
    items
        .iter()
        .filter_map(|item| {
            item.composed_selector()
                .map(|selector| (selector, item.syntax().to_owned()))
        })
        .collect()
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
    [(get_composed_selectors(contract.constructors()), "constructor"), (get_composed_selectors(contract.messages()), "message")].iter().flat_map(|(selectors, name)| {
        let mut seen_selectors: HashSet<u32> = HashSet::new();
        selectors.iter().filter_map(|(selector, node)| {
            let selector_value = selector.into_be_u32();
            if seen_selectors.get(&selector_value).is_some() {
                return Some(Diagnostic {
                    message: format!("Selector values must be unique across all ink! {name}s in an ink! contract."),
                    range: node.text_range(),
                    severity: Severity::Error,
                });
            }
            seen_selectors.insert(selector_value);
            None
        }).collect::<Vec<Diagnostic>>()
    }).collect()
}

/// Returns all ink! selector arguments for a list of ink! callable entities.
fn get_selector_args<T>(items: &[T]) -> Vec<SelectorArg>
where
    T: InkItem,
{
    items
        .iter()
        .flat_map(|item| {
            item.ink_args_by_kind(InkArgKind::Selector)
                .into_iter()
                .filter_map(SelectorArg::cast)
        })
        .collect()
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
    [(get_selector_args(contract.constructors()), "constructor"), (get_selector_args(contract.messages()), "message")].iter().flat_map(|(selectors, name)| {
        let mut has_seen_wildcard = false;
        selectors.iter().filter_map(|selector| {
            if selector.is_wildcard() {
                if has_seen_wildcard {
                    return Some(Diagnostic {
                        message: format!("At most one wildcard (`_`) selector can be defined across all ink! {name}s in an ink! contract."),
                        range: selector.text_range(),
                        severity: Severity::Error,
                    });
                }
                has_seen_wildcard = true;
            }
            None
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

/// Ensure ink! impls are defined in the root of the ink! contract.
///
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L88-L97>.
fn ensure_impls_in_root(contract: &Contract) -> Vec<Diagnostic> {
    contract
        .impls()
        .iter()
        .filter_map(|item| {
            let is_parent = if let Some(parent_contract) =
                ink_analyzer_ir::ink_parent::<Contract>(item.syntax())
            {
                parent_contract.syntax() == contract.syntax()
            } else {
                false
            };

            (!is_parent).then_some(Diagnostic {
                message:
                    "ink! impls must be defined in the root of the ink! contract's `mod` item."
                        .to_string(),
                range: item.syntax().text_range(),
                severity: Severity::Error,
            })
        })
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
                            #[ink(constructor, payable, default, selector=0x1)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=0x1)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
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
                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message(&self) {}
                        }
                    }
                },
                // Minimal + Event + Multiple Selectors.
                // Overlaps between constructors and messages are ok.
                // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L838-L857>.
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

                            #[ink(constructor, payable, default, selector=2)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=2)]
                            pub fn minimal_message2(&self) {}
                        }
                    }
                },
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
                            #[ink(constructor, payable, default, selector=0x1)]
                            pub fn new() -> Self {}

                            #[ink(constructor, payable, default, selector=0x2)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=0x1)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=0x2)]
                            pub fn minimal_message2(&self) {}
                        }
                    }
                },
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
                            #[ink(constructor, payable, default)]
                            pub fn new() -> Self {}

                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new2() -> Self {}

                            #[ink(constructor, payable, default, selector=3)]
                            pub fn new3() -> Self {}

                            #[ink(constructor, payable, default, selector=0x4)]
                            pub fn new4() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message(&self) {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message2(&self) {}

                            #[ink(message, payable, default, selector=3)]
                            pub fn minimal_message3(&self) {}

                            #[ink(message, payable, default, selector=0x4)]
                            pub fn minimal_message4(&self) {}
                        }
                    }
                },
                // Minimal + Event + Multiple Selectors + Traits + Namespace + Impl attribute.
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
                            #[ink(constructor, payable, default)]
                            pub fn new() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message(&self) {}

                            #[ink(constructor, payable, default, selector=_)]
                            pub fn new2() -> Self {}

                            #[ink(message, payable, default, selector=_)]
                            pub fn minimal_message2(&self) {}

                            #[ink(constructor, payable, default, selector=3)]
                            pub fn new3() -> Self {}

                            #[ink(constructor, payable, default, selector=0x4)]
                            pub fn new4() -> Self {}

                            #[ink(message, payable, default, selector=3)]
                            pub fn minimal_message3(&self) {}

                            #[ink(message, payable, default, selector=0x4)]
                            pub fn minimal_message4(&self) {}
                        }

                        impl MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new5() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message5(&self) {}
                        }

                        impl ::my_full::long_path::MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new6() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message6(&self) {}
                        }

                        impl relative_path::MyTrait for Minimal {
                            #[ink(constructor, payable, default)]
                            fn new7() -> Self {}

                            #[ink(message, payable, default)]
                            fn minimal_message7(&self) {}
                        }

                        #[ink(namespace="my_namespace")]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new8() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message8(&self) {}
                        }

                        #[ink(impl)]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new9() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message9(&self) {}
                        }

                        #[ink(impl, namespace="my_namespace")]
                        impl Minimal {
                            #[ink(constructor, payable, default)]
                            pub fn new10() -> Self {}

                            #[ink(message, payable, default)]
                            pub fn minimal_message10(&self) {}
                        }
                    }
                },
                // Minimal + Tests.
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

                        #[cfg(test)]
                        mod tests {
                            #[ink::test]
                            fn it_works() {
                            }
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

                            #[ink(message, selector=2)]
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
            assert!(result.is_none(), "contract: {}", code);
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
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_storage_quantity(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
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
    fn one_or_multiple_constructors_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_contains_constructor(&contract);
            assert!(result.is_none(), "contract: {}", code);
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
    fn one_or_multiple_messages_works() {
        // Tests snippets with btn 2 and 5 messages.
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let result = ensure_contains_message(&contract);
            assert!(result.is_none(), "contract: {}", code);
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
    fn non_overlapping_selectors_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_no_overlapping_selectors(&contract);
            assert!(results.is_empty(), "contract: {}", code);
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
        ]
        .iter()
        .map(|item| {
            quote! {
                impl MyContract {
                    #item
                }
            }
        })
        .chain([
            // Overlapping trait implementations.
            // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L810-L836>.
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
        ]) {
            let contract = parse_first_contract(quote_as_str! {
                #[ink::contract]
                mod my_contract {
                    #code
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
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L883-L902>.
    fn one_or_no_wildcard_selectors_works() {
        for code in valid_contracts!() {
            // At most one wildcard is allowed for each group i.e there can be messages and constructors
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_at_most_one_wildcard_selector(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
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
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_impl_parent_for_callables(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
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
    fn impls_in_root_works() {
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_impls_in_root(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
    }

    #[test]
    fn impls_not_in_root_fails() {
        let contract = parse_first_contract(quote_as_str! {
            #[ink::contract]
            mod my_contract {
                fn impl_container() {
                    pub struct MyImpl;

                    impl MyImpl {
                        #[ink(constructor)]
                        pub fn my_constructor() -> Self {
                        }

                        #[ink(message)]
                        pub fn my_message() {
                        }
                    }

                    #[ink(impl)]
                    impl MyImpl {
                    }
                }
            }
        });

        let results = ensure_impls_in_root(&contract);

        // There should be 2 errors (i.e one for each of the impls).
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
        for code in valid_contracts!() {
            let contract = parse_first_contract(quote_as_str! {
                #code
            });

            let results = ensure_valid_quasi_direct_ink_descendants(&contract);
            assert!(results.is_empty(), "contract: {}", code);
        }
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
