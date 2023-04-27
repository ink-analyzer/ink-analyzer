//! ink! impl diagnostics.

use ink_analyzer_ir::ast::{AstNode, HasVisibility, Type};
use ink_analyzer_ir::{
    ast, AsInkFn, AsInkImplItem, FromSyntax, Impl, InkArgKind, InkAttributeKind,
};

use super::{constructor, message, utils};
use crate::{Diagnostic, Severity};

/// Runs all ink! impl diagnostics.
///
/// The entry point for finding ink! impl semantic rules is the item_impl module of the ink_ir crate.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221-L334>.
pub fn diagnostics(impl_item: &Impl) -> Vec<Diagnostic> {
    let mut results: Vec<Diagnostic> = Vec::new();

    // Run generic diagnostics, see `utils::run_generic_diagnostics` doc.
    utils::append_diagnostics(&mut results, &mut utils::run_generic_diagnostics(impl_item));

    // Ensure ink! impl is an `impl` item, see `ensure_impl` doc.
    if let Some(diagnostic) = ensure_impl(impl_item) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure `impl` item satisfies all invariants of an ink! impl,
    // see `ensure_impl_invariants` doc.
    utils::append_diagnostics(&mut results, &mut ensure_impl_invariants(impl_item));

    // Ensure at least one ink! constructor or ink! message, see `ensure_contains_callable` doc.
    if let Some(diagnostic) = ensure_contains_callable(impl_item) {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Run ink! constructor diagnostics, see `constructor::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut impl_item
            .constructors()
            .iter()
            .flat_map(constructor::diagnostics)
            .collect(),
    );

    // Run ink! message diagnostics, see `message::diagnostics` doc.
    utils::append_diagnostics(
        &mut results,
        &mut impl_item
            .messages()
            .iter()
            .flat_map(message::diagnostics)
            .collect(),
    );

    // Ensure ink! messages and constructors are defined in the root of an `impl` item,
    // see `ensure_impl_parent_for_callables` doc.
    utils::append_diagnostics(&mut results, &mut ensure_callables_in_root(impl_item));

    // Ensure ink! impl is defined in the root of an ink! contract, see `utils::ensure_contract_parent` doc.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item/mod.rs#L88-L97>.
    if let Some(diagnostic) = utils::ensure_contract_parent(impl_item, "impl") {
        utils::push_diagnostic(&mut results, diagnostic);
    }

    // Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors),
    // See `ensure_valid_quasi_direct_ink_descendants` doc.
    utils::append_diagnostics(
        &mut results,
        &mut ensure_valid_quasi_direct_ink_descendants(impl_item),
    );

    results
}

/// Ensure ink! impl is an `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221>.
fn ensure_impl(impl_item: &Impl) -> Option<Diagnostic> {
    impl_item.impl_item().is_none().then_some(Diagnostic {
        message: "ink! impls must be `impl` items".to_string(),
        range: impl_item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensure `impl` satisfies all invariants of an ink! impl.
///
/// See references below for details about checked invariants.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L221-L334>.
pub fn ensure_impl_invariants(impl_item: &Impl) -> Vec<Diagnostic> {
    let mut results = Vec::new();

    if let Some(ast_impl_item) = impl_item.impl_item() {
        if let Some(default_token) = ast_impl_item.default_token() {
            results.push(Diagnostic {
                message: "ink! impls must not be `default`.".to_string(),
                range: default_token.text_range(),
                severity: Severity::Error,
            });
        }

        if let Some(unsafe_token) = ast_impl_item.unsafe_token() {
            results.push(Diagnostic {
                message: "ink! impls must not be `unsafe`.".to_string(),
                range: unsafe_token.text_range(),
                severity: Severity::Error,
            });
        }

        if let Some(diagnostic) = utils::ensure_no_generics(&ast_impl_item, "impl") {
            results.push(diagnostic);
        }

        if let Some(Type::PathType(path_type)) = ast_impl_item.self_ty() {
            if let Some(path) = path_type.path() {
                results.append(
                    &mut path
                        .segments()
                        .filter_map(|arg| {
                            arg.generic_arg_list().map(|generic_arg_list| Diagnostic {
                                message: "Generic types on ink! impls are not supported."
                                    .to_string(),
                                range: generic_arg_list.syntax().text_range(),
                                severity: Severity::Error,
                            })
                        })
                        .collect(),
                );
            }
        }

        let is_trait_impl = ast_impl_item.trait_().is_some();
        if is_trait_impl && impl_item.namespace_arg().is_some() {
            results.push(Diagnostic {
                message: "ink! namespace argument is not allowed on trait ink! impl blocks."
                    .to_string(),
                range: impl_item
                    .namespace_arg()
                    .expect("Namespace should exist at this point.")
                    .text_range(),
                severity: Severity::Error,
            });
        }

        let constructor_fns: Vec<&ast::Fn> = impl_item
            .constructors()
            .iter()
            .filter_map(|item| item.fn_item())
            .collect();
        let message_fns: Vec<&ast::Fn> = impl_item
            .messages()
            .iter()
            .filter_map(|item| item.fn_item())
            .collect();
        results.append(&mut [(constructor_fns, "constructor"), (message_fns, "message")].iter().flat_map(|(fns, name)| {
            fns.iter().filter_map(move |fn_item| {
                if is_trait_impl {
                    // Callables must have inherent visibility for trait implementation blocks.
                    fn_item.visibility().map(|visibility| Diagnostic {
                        message: format!("ink! {name}s in trait ink! impl blocks must have inherited visibility."),
                        range: visibility.syntax().text_range(),
                        severity: Severity::Error,
                    })
                } else {
                    // Callables must have `pub` visibility for inherent implementation blocks.
                    let (has_pub_visibility, visibility) = if let Some(visibility) = fn_item.visibility() {
                        (visibility.syntax().to_string() == "pub", Some(visibility))
                    } else {
                        // Inherited visibility.
                        (false, None)
                    };

                    (!has_pub_visibility).then_some(Diagnostic {
                        message: format!("ink! {name}s in inherent ink! impl blocks must have `pub` visibility."),
                        range: if let Some(vis) = visibility {
                            vis.syntax().text_range()
                        } else {
                            fn_item.syntax().text_range()
                        },
                        severity: Severity::Error,
                    })
                }
            })
        }).collect());
    }

    results
}

/// Ensure at least one ink! constructor or ink! message.
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/mod.rs#L189-L208>.
fn ensure_contains_callable(impl_item: &Impl) -> Option<Diagnostic> {
    (impl_item.constructors().is_empty() && impl_item.messages().is_empty()).then_some(Diagnostic {
        message: "At least one ink! constructor or ink! message must be defined for an ink! impl."
            .to_string(),
        range: impl_item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensure item is defined in the root of this specific `impl` item.
fn ensure_parent_impl<T>(impl_item: &Impl, item: &T, ink_scope_name: &str) -> Option<Diagnostic>
where
    T: AsInkImplItem + FromSyntax,
{
    let is_parent = if let Some(parent_ast_impl) = item.impl_item() {
        parent_ast_impl.syntax() == impl_item.syntax()
    } else {
        false
    };

    (!is_parent).then_some(Diagnostic {
        message: format!(
            "ink! {ink_scope_name}s must be defined in the ink! contract's `impl` block."
        ),
        range: item.syntax().text_range(),
        severity: Severity::Error,
    })
}

/// Ensure ink! messages and constructors are defined in the root of the `impl` item.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L410-L469>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/constructor.rs#L36-L66>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/message.rs#L66-L96>.
///
/// Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_impl/impl_item.rs#L64-L87>.
fn ensure_callables_in_root(impl_item: &Impl) -> Vec<Diagnostic> {
    impl_item
        .constructors()
        .iter()
        .filter_map(|item| ensure_parent_impl(impl_item, item, "constructor"))
        .chain(
            impl_item
                .messages()
                .iter()
                .filter_map(|item| ensure_parent_impl(impl_item, item, "message")),
        )
        .collect()
}

/// Ensure only valid quasi-direct ink! attribute descendants (i.e ink! descendants without any ink! ancestors).
///
/// Ref: <https://github.com/paritytech/ink/blob/master/crates/ink/ir/src/ir/item_impl/impl_item.rs#L62-L106>.
fn ensure_valid_quasi_direct_ink_descendants(impl_item: &Impl) -> Vec<Diagnostic> {
    utils::ensure_valid_quasi_direct_ink_descendants(impl_item, |attr| {
        matches!(
            attr.kind(),
            InkAttributeKind::Arg(InkArgKind::Constructor)
                | InkAttributeKind::Arg(InkArgKind::Message)
                | InkAttributeKind::Arg(InkArgKind::Payable)
                | InkAttributeKind::Arg(InkArgKind::Default)
                | InkAttributeKind::Arg(InkArgKind::Selector)
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ink_analyzer_ir::{quote_as_str, InkFile};
    use quote::quote;

    fn parse_first_impl_item(code: &str) -> Impl {
        InkFile::parse(code)
            .syntax()
            .descendants()
            .find_map(Impl::cast)
            .unwrap()
    }

    // List of valid minimal ink! impls used for positive(`works`) tests for ink! impl verifying utilities.
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    macro_rules! valid_impl_items {
        () => {
            [
                // Simple.
                quote! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn new() -> Self {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(message)]
                        pub fn minimal_message(&self) {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(constructor)]
                        pub fn new() -> Self {}

                        #[ink(message)]
                        pub fn minimal_message(&self) {}
                    }
                },
                // Args.
                quote! {
                    impl MyContract {
                        #[ink(constructor, payable, default, selector=1)]
                        pub fn new() -> Self {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(message, payable, default, selector=1)]
                        pub fn minimal_message(&self) {}
                    }
                },
                quote! {
                    impl MyContract {
                        #[ink(constructor, payable, default, selector=1)]
                        pub fn new() -> Self {}

                        #[ink(message, payable, default, selector=1)]
                        pub fn minimal_message(&self) {}
                    }
                },
            ]
            .iter()
            .flat_map(|code| {
                [
                    // Simple.
                    quote! {
                        #code
                    },
                    // Impl attribute.
                    quote! {
                        #[ink(impl)]
                        #code
                    },
                    // Namespace Attr.
                    quote! {
                        #[ink(namespace="my_namespace")]
                        #code
                    },
                    // Compound.
                    quote! {
                        #[ink(impl, namespace="my_namespace")]
                        #code
                    },
                ]
            })
            .chain(
                [
                    // Traits.
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor)]
                            fn new() -> Self {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(message)]
                            fn minimal_message(&self) {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor)]
                            fn new() -> Self {}

                            #[ink(message)]
                            fn minimal_message(&self) {}
                        }
                    },
                    // Traits + Args.
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor, payable, default, selector=1)]
                            fn new() -> Self {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(message, payable, default, selector=1)]
                            fn minimal_message(&self) {}
                        }
                    },
                    quote! {
                        impl MyTrait for MyContract {
                            #[ink(constructor, payable, default, selector=1)]
                            fn new() -> Self {}

                            #[ink(message, payable, default, selector=1)]
                            fn minimal_message(&self) {}
                        }
                    },
                ]
                .iter()
                .flat_map(|code| {
                    // Namespace shouldn't be used for trait implementations.
                    [
                        // Simple.
                        quote! {
                            #code
                        },
                        // Impl attribute.
                        quote! {
                            #[ink(impl)]
                            #code
                        },
                    ]
                }),
            )
            // Wrap in contract for context sensitive tests.
            .map(|items| {
                quote! {
                    #[ink::contract]
                    mod my_contract {
                        #items
                    }
                }
            })
        };
    }

    #[test]
    fn impl_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let result = ensure_impl(&impl_item);
            assert!(result.is_none());
        }
    }

    #[test]
    fn non_impl_fails() {
        for code in [
            quote! {
                mod my_impl {
                }
            },
            quote! {
                fn my_impl() {
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
            let impl_item = parse_first_impl_item(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                #code
            });

            let result = ensure_impl(&impl_item);
            assert!(result.is_some(), "impl: {}", code);
            assert_eq!(result.unwrap().severity, Severity::Error, "impl: {}", code);
        }
    }

    #[test]
    fn valid_impl_properties_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let results = ensure_impl_invariants(&impl_item);
            assert!(results.is_empty(), "impl: {}", code);
        }
    }

    #[test]
    fn invalid_impl_properties_fails() {
        for code in [
            // Default.
            quote! {
                default impl MyContract {}
            },
            // Unsafe.
            quote! {
                unsafe impl MyContract {}
            },
            // Generic.
            quote! {
                impl MyContract<T> {}
            },
            // Trait implementations with namespace.
            quote! {
                #[ink(namespace="my_namespace")]
                impl MyTrait for MyContract {}
            },
            // Trait implementations pub visibility for callables.
            quote! {
                impl MyTrait for MyContract {
                    #[ink(constructor)]
                    pub fn new() -> Self {}
                }
            },
            quote! {
                impl MyTrait for MyContract {
                    #[ink(message)]
                    pub fn minimal_message(&self) {}
                }
            },
            // Inherent implementations inherited visibility for callables.
            quote! {
                impl MyContract {
                    #[ink(constructor)]
                    fn new() -> Self {}
                }
            },
            quote! {
                impl MyContract {
                    #[ink(message)]
                    fn minimal_message(&self) {}
                }
            },
        ] {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                #code
            });

            let results = ensure_impl_invariants(&impl_item);
            assert_eq!(results.len(), 1, "impl: {}", code);
            assert_eq!(results[0].severity, Severity::Error, "impl: {}", code);
        }
    }

    #[test]
    fn ensure_contains_callables_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let result = ensure_contains_callable(&impl_item);
            assert!(result.is_none(), "impl: {}", code);
        }
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L688-L704>.
    fn missing_callable_fails() {
        let impl_item = parse_first_impl_item(quote_as_str! {
            #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
            impl MyContract {
            }
        });

        let result = ensure_contains_callable(&impl_item);
        assert!(result.is_some());
        assert_eq!(result.unwrap().severity, Severity::Error);
    }

    #[test]
    fn impl_parent_for_callables_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let results = ensure_callables_in_root(&impl_item);
            assert!(results.is_empty(), "impl: {}", code);
        }
    }

    #[test]
    fn non_impl_parent_for_callables_fails() {
        for code in [
            quote! {
                fn callable_container() {
                    #[ink(constructor)]
                    pub fn my_constructor() -> i32 {
                    }

                    #[ink(message)]
                    pub fn my_message() {
                    }
                }
            },
            quote! {
                fn callable_container() {
                    struct MyStruct;

                    impl MyStruct {
                        #[ink(constructor)]
                        pub fn my_constructor() -> i32 {
                        }

                        #[ink(message)]
                        pub fn my_message() {
                        }
                    }

                }
            },
        ] {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
                impl MyContract {
                    #code
                }
            });

            let results = ensure_callables_in_root(&impl_item);

            // There should be 2 errors (i.e for the `constructor` and `message`).
            assert_eq!(results.len(), 2, "impl: {}", code);
            // All diagnostics should be errors.
            assert_eq!(
                results
                    .iter()
                    .filter(|item| item.severity == Severity::Error)
                    .count(),
                2,
                "impl: {}",
                code
            );
        }
    }

    #[test]
    fn valid_quasi_direct_descendant_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let results = ensure_valid_quasi_direct_ink_descendants(&impl_item);
            assert!(results.is_empty(), "impl: {}", code);
        }
    }

    #[test]
    fn invalid_quasi_direct_descendant_fails() {
        let impl_item = parse_first_impl_item(quote_as_str! {
            #[ink(impl)] // needed for this to parsed as an ink! impl without messages and constructors.
            impl MyContract {
                #[ink(storage)]
                struct MyContract {
                }

                #[ink(event)]
                struct MyEvent {
                }

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

        let results = ensure_valid_quasi_direct_ink_descendants(&impl_item);
        // There should be 5 errors (i.e `storage`, `event`, `trait_definition`, `chain_extension` and `storage_item`).
        assert_eq!(results.len(), 5);
        // All diagnostics should be errors.
        assert_eq!(
            results
                .iter()
                .filter(|item| item.severity == Severity::Error)
                .count(),
            5
        );
    }

    #[test]
    // Ref: <https://github.com/paritytech/ink/blob/v4.1.0/crates/ink/ir/src/ir/item_mod.rs#L593-L640>.
    fn compound_diagnostic_works() {
        for code in valid_impl_items!() {
            let impl_item = parse_first_impl_item(quote_as_str! {
                #code
            });

            let results = diagnostics(&impl_item);
            assert!(results.is_empty(), "impl: {}", code);
        }
    }
}
